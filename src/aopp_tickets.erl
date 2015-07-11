-module(aopp_tickets).
-behaviour(gen_server).

-export([start/0]).
-export([set_base_url/1]).
-export([set_credentials/2]).
-export([test_credentials/0]).
-export([set_story_points/1]).
-export([current/0]).
-export([jump_to/1]).
-export([load/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {current :: aopp_ticket:ticket()
                ,votes = dict:new() :: dict:dict()
                ,vote_complete = 'false' :: boolean()
                ,authorization :: list()
                ,base_url = "https://2600hz.atlassian.net" :: list()
                ,start_at = 0 :: non_neg_integer()
                ,max_results = 10 :: pos_integer()
                ,tickets = dict:new() :: dict:dict()
               }).
-type state() :: #state{}.

-spec start() -> {'ok', pid()} | 'ignore' | {'error', _}.
start() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec set_base_url(binary()) -> 'ok'.
set_base_url(Url) ->
    gen_server:cast(?MODULE, {'base_url', binary_to_list(Url)}).

-spec set_credentials(binary(), binary()) -> 'ok'.
set_credentials(Username, Password) ->
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    gen_server:cast(?MODULE, {'authorization', Encoded}).

-spec test_credentials() -> {'ok', _} | {'error', binary()} | {'errors', [binary()]}.
test_credentials() ->
    gen_server:call(?MODULE, 'test_credentials').

-spec set_story_points(non_neg_integer()) -> 'ok'.
set_story_points(StoryPoints) ->
    gen_server:cast(?MODULE, {'set_story_points', StoryPoints}).

-spec current() -> aopp_ticket:ticket() | 'undefined'.
current() ->
    gen_server:call(?MODULE, 'current').

-spec jump_to(binary()) -> 'ok'.
jump_to(Key) ->
    gen_server:call(?MODULE, {'jump_to', Key}).

-spec load(binary()) -> 'ok'.
load(JQL) ->
    gen_server:call(?MODULE, {'load', JQL}).

-spec init(_) -> {'ok', state()}.
init(_Args) ->
    {'ok', #state{}}.

-spec handle_call(_, {pid(), _}, state()) -> {'reply', _, state()}.
handle_call('current', _, State) ->
    {'reply', current_ticket(State), State};
handle_call('test_credentials', _, State) ->
    {'reply', test_authentication(State), State};
handle_call({'load', JQL}, _, State) ->
    case query(JQL, State) of
        {'ok', Tickets}=Ok ->
            {'reply', Ok, set_tickets(Tickets, reset(State))};
        Else -> {'reply', Else, State}
    end;
handle_call({'jump_to', Key}, _, State) ->
    case fetch(Key, State) of
        {'ok', Ticket}=Ok ->
            {'reply', Ok, set_current_ticket(Ticket, State)};
        Else -> {'reply', Else, State}
    end;
handle_call(_Request, _From, State) ->
    {'reply', 'not_implemented', State}.

-spec handle_cast(_, state()) -> {'noreply', state()}.
handle_cast({'authorization', Authorization}, State) ->
    {'noreply', set_authorization(Authorization, State)};
handle_cast({'base_url', Url}, State) ->
    {'noreply', set_base_url(Url, State)};
handle_cast({'set_story_points', StoryPoints}, State) ->
    {'noreply', set_story_points(StoryPoints, State)};
handle_cast(_Request, State) ->
    {'noreply', State}.

-spec handle_info(_, state()) -> {'noreply', state()}.
handle_info(_Message, State) ->
    {'noreply', State}.

-spec terminate(_, state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

-spec code_change(_, state(), _) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec set_authorization(list(), state()) -> state().
set_authorization(Authorization, State) ->
    State#state{authorization=Authorization}.

-spec set_base_url(list(), state()) -> state().
set_base_url(Url, State) ->
    State#state{base_url=Url}.

-spec current_ticket(state()) -> aopp_ticket:ticket() | 'undefined'.
current_ticket(#state{current=Ticket}) ->
    Ticket.

-spec set_current_ticket(aopp_ticket:ticket(), state()) -> state().
set_current_ticket(Ticket, #state{tickets=Tickets}=State) ->
    _ = aopp_participants:broadcast_event(
          <<"current_ticket">>
          ,aopp_ticket:to_map(Ticket)
         ),
    _ = aopp_votes:reset(),
    Key = aopp_ticket:key(Ticket),
    State#state{current=Ticket
               ,tickets=dict:store(Key, Ticket, Tickets)
               }.

-spec fetch(binary(), state()) -> state().
fetch(Key, State) ->
    case ticket_url(Key, State) of
        {'error', _}=Error -> Error;
        {'ok', Url} ->
            _ = aopp_participants:broadcast_event(<<"loading_ticket">>, #{}),
            case make_request('get', Url, 'undefined', State) of
                {'ok', JObj} -> {'ok', aopp_ticket:from_map(JObj)};
                Else -> Else
            end
    end.

-spec test_authentication(state()) -> {'ok', [map()]} | {'error', binary()} | {'errors', [binary()]}.
test_authentication(State) ->
    Url = projects_url(State),
    make_request('get', Url, 'undefined', State).

-spec query(binary(), state()) -> {'ok', [map()]} | {'error', binary()} | {'errors', [binary()]}.
query(JQL, #state{max_results=MaxResults
                 ,start_at=StartAt
                 }=State) ->
    Url = search_url(State),
    Body =
        jiffy:encode(
          #{<<"jql">> => JQL
           ,<<"startAt">> => StartAt
           ,<<"maxResults">> => MaxResults
           ,<<"fields">> =>
                [<<"key">>
                ,<<"summary">>
                ,<<"customfield_10002">>
                ]
           }
         ),
    case make_request('post', Url, Body, State) of
        {'ok', JObjs} ->
            Tickets =
                [aopp_ticket:from_map(JObj)
                 || JObj <- maps:get(<<"issues">>, JObjs, [])
                ],
            {'ok', Tickets};
        Else -> Else
    end.

-spec make_request(atom(), list(), binary(), state()) -> {'ok', [map()]} | {'error', binary()} | {'errors', [binary()]}.
make_request(Method, Url, Body, State) ->
    Headers = [auth_header(State)],
    Request =
        case Body =:= 'undefined' of
            'true' -> {Url, Headers};
            'false' ->
                {Url, Headers, "application/json", Body}
        end,
    case httpc:request(Method, Request, [], []) of
        {'ok', {{_, Code, Message}, _, Result}} ->
            handle_request_result(Code, Message, Result);
        {'error', _Reason} ->
            {'error', <<"Request to ", (list_to_binary(Url))/binary, " failed">>}
    end.

handle_request_result(Code, Message, Result) when not is_binary(Message) ->
    handle_request_result(Code, list_to_binary(Message), Result);
handle_request_result(Code, Message, Result) when is_list(Result) ->
    try jiffy:decode(Result, ['return_maps']) of
        JObj -> handle_request_result(Code, Message, JObj)
    catch
        _:_ -> handle_request_result(Code, Message, #{})
    end;
handle_request_result(Code, Message, JObj) when Code >= 400 ->
    Errors = maps:get(<<"errorMessages">>, JObj, [Message]),
    {'errors', Errors};
handle_request_result(Code, _, JObj) when Code >= 200, Code < 300 ->
    {'ok', JObj};
handle_request_result(_, _, _) ->
    {'errors', [<<"Unexpected Jira response">>]}.

-spec search_url(state()) -> list().
search_url(#state{base_url=Url}) ->
    Url ++ "/rest/api/2/search".

-spec projects_url(state()) -> list().
projects_url(#state{base_url=Url}) ->
    Url ++ "/rest/api/2/project".

-spec ticket_url(binary(), state()) -> list().
ticket_url(Key, #state{tickets=Tickets}) ->
    case dict:find(Key, Tickets) of
        {'ok', Ticket} ->
            {'ok', binary_to_list(aopp_ticket:self(Ticket))};
        'error' ->
            {'error', <<"Unknown ticket">>}
    end.

-spec auth_header(state()) -> {list(), list()}.
auth_header(#state{authorization=Authorization}) ->
    {"Authorization","Basic " ++ Authorization}.

-spec set_tickets([map()] | map(), state()) -> map().
set_tickets([], State) -> State;
set_tickets([Ticket|Tickets], #state{tickets=Dict}=State) ->
    Key = aopp_ticket:key(Ticket),
    set_tickets(Tickets, State#state{tickets=dict:store(Key, Ticket, Dict)}).

-spec reset(state()) -> state().
reset(State) ->
    State#state{tickets=dict:new()
               ,votes=dict:new()
               ,vote_complete='false'
               ,current='undefined'
               ,start_at=0
               }.

-spec set_story_points(non_neg_integer(), state()) -> state().
set_story_points(StoryPoints, #state{tickets=Tickets
                               ,current=Current
                               }=State) ->
    Ticket = aopp_ticket:set_story_points(StoryPoints, Current),
    Key = aopp_ticket:key(Ticket),
    case update_story_points(Key, StoryPoints, State) of
        {'ok', _} ->
            Message = <<"Set ", Key/binary
                        ," story points to "
                        ,(aopp_ticket:story_points_binary(Ticket))/binary
                      >>,
            _ = aopp_participants:broadcast_event(
                  <<"success">>
                  ,#{<<"message">> => Message}
                 ),
            _ = aopp_participants:broadcast_event(
                  <<"update_ticket">>
                  ,aopp_ticket:to_map(Ticket)
                 ),
            State#state{current=Ticket
                       ,tickets=dict:store(Key, Ticket, Tickets)
                       };
        _Else ->
            Message = <<"Failed to set ", Key/binary
                        ," story points to "
                        ,(aopp_ticket:story_points_binary(StoryPoints))/binary
                      >>,
            _ = aopp_participants:broadcast_event(
                  <<"error">>
                  ,#{<<"message">> => Message}
                 ),
            State
    end.

-spec update_story_points(binary(), non_neg_integer(), state()) ->
                                 {'ok', [map()]} |
                                 {'error', binary()} |
                                 {'errors', [binary()]}.
update_story_points(Key, StoryPoints, State) ->
    Data = #{<<"update">> =>
                 #{<<"customfield_10002">> =>
                       [#{<<"set">> => StoryPoints}]
                  }
            },
    Body = jiffy:encode(Data),
    case ticket_url(Key, State) of
        {'ok', Url} ->
            make_request('put', Url, Body, State);
        {'error', _}=Error -> Error
    end.
