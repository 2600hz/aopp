-module(aopp_votes).
-behaviour(gen_server).

-export([start/0]).
-export([summarize/0]).
-export([update/2]).
-export([remove/1]).
-export([reset/0]).
-export([enable/0
        ,disable/0
        ]).
-export([show_values/0
         ,hide_values/0
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {votes = dict:new() :: dict:dict()
                ,enabled = 'false' :: boolean()
                ,values_visible = 'false' :: boolean()
               }).
-type state() :: #state{}.
-record(vote, {participant :: aopp_participant:participant()
              ,value :: non_neg_integer()
              }).
-type vote() :: #vote{}.

-spec start() -> {'ok', pid()} | 'ignore' | {'error', _}.
start() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec summarize() -> map().
summarize() ->
    gen_server:call(?MODULE, 'summarize').

-spec update(aopp_participant:participant(), non_neg_integer()) -> 'exists' | 'ok'.
update(Participant, Value) ->
    gen_server:call(?MODULE, {'update', Participant, Value}).

-spec remove(aopp_participant:participant()) -> 'ok'.
remove(Participant) ->
    gen_server:cast(?MODULE, {'remove', Participant}).

-spec reset() -> 'ok'.
reset() ->
    gen_server:cast(?MODULE, 'reset').

-spec enable() -> 'ok'.
enable() ->
    gen_server:cast(?MODULE, 'enable').

-spec disable() -> 'ok'.
disable() ->
    gen_server:cast(?MODULE, 'disable').

-spec show_values() -> 'ok'.
show_values() ->
    gen_server:cast(?MODULE, 'show_values').

-spec hide_values() -> 'ok'.
hide_values() ->
    gen_server:cast(?MODULE, 'hide_values').

-spec init(_) -> {'ok', state()}.
init(_Args) ->
    {'ok', #state{}}.

-spec handle_call(_, {pid(), _}, state()) -> {'reply', _, state()}.
handle_call({'update', _, _}, _, #state{enabled='false'}=State) ->
    {'reply', 'disabled', State};
handle_call({'update', Participant, Value}, _, State) ->
    {'reply', 'ok', update(Participant, Value, State)};
handle_call('summarize', _, State) ->
    {'reply', summarize(State), State};
handle_call(_Request, _From, State) ->
    {'reply', 'not_implemented', State}.

-spec handle_cast(_, state()) -> {'noreply', state()}.
handle_cast('reset', State) ->
    {'noreply', reset(State)};
handle_cast('enable', State) ->
    {'noreply', enable(State)};
handle_cast('disable', State) ->
    {'noreply', disable(State)};
handle_cast('show_values', State) ->
    {'noreply', show_values(State)};
handle_cast('hide_values', State) ->
    {'noreply', hide_values(State)};
handle_cast({'remove', Participant}, State) ->
    {'noreply', remove(Participant, State)};
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

-spec update(aopp_participant:participant(), non_neg_integer(), state()) -> state().
update(Participant, Value, #state{votes=Votes}=State) ->
    Id = aopp_participant:id(Participant),
    NewVotes = dict:store(Id, create(Participant, Value), Votes),
    maybe_vote_complete(State#state{votes=NewVotes}).

-spec remove(aopp_participant:participant(), state()) -> state().
remove(Participant, #state{votes=Votes}=State) ->
    Id = aopp_participant:id(Participant),
    NewVotes = dict:erase(Id, Votes),
    maybe_vote_complete(State#state{votes=NewVotes}).

-spec maybe_vote_complete(state()) -> state().
maybe_vote_complete(#state{votes=Votes}=State) ->
    case dict:size(Votes) =:= aopp_participants:count() of
        'true' -> vote_complete(State);
        'false' -> broadcast_update(State)
    end.

-spec vote_complete(state()) -> state().
vote_complete(State) ->
    broadcast_update(
      State#state{enabled='false'
                 ,values_visible='true'
                 }
     ).

-spec create(aopp_participant:participant(), non_neg_integer()) -> vote().
create(Participant, Value) ->
    #vote{participant=Participant
          ,value=Value
         }.

-spec enable(state()) -> state().
enable(State) ->
    broadcast_update(
      State#state{enabled='true'}
     ).

-spec disable(state()) -> state().
disable(State) ->
    broadcast_update(
      State#state{enabled='false'}
     ).

-spec show_values(state()) -> state().
show_values(State) ->
    broadcast_update(
      State#state{values_visible='true'}
     ).

-spec hide_values(state()) -> state().
hide_values(State) ->
    broadcast_update(
      State#state{values_visible='false'}
     ).

-spec broadcast_update(state()) -> state().
broadcast_update(State) ->
    broadcast_update(<<"votes_update">>, State).

-spec broadcast_update(binary(), state()) -> state().
broadcast_update(Event, State) ->
    _ = aopp_participants:broadcast_event(
          Event
          ,summarize(State)
         ),
    State.

-spec summarize(state()) -> map().
summarize(#state{enabled=Enabled
                ,values_visible=Visible
                ,votes=Votes
                }=State)->
    #{<<"enabled">> => Enabled
      ,<<"values_visible">> => Visible
      ,<<"votes">> =>
          [vote_summary(Vote, State)
           || {_, Vote} <- dict:to_list(Votes)
          ]
     }.

-spec vote_summary(vote(), state()) -> map().
vote_summary(#vote{participant=Participant}
            ,#state{values_visible='false'}) ->
    aopp_participant:to_map(Participant);
vote_summary(#vote{participant=Participant
                  ,value=Value}
            ,#state{values_visible='true'}) ->
    maps:put(
      <<"value">>
      ,Value
      ,aopp_participant:to_map(Participant)
     ).

-spec reset(state()) -> state().
reset(State) ->
    broadcast_update(
      State#state{votes=dict:new()
                 ,enabled='true'
                 ,values_visible='false'
                 }
     ).
