-module(aopp_api).

-export([initial/1]).
-export([promote/1
        ,update_name/1
        ]).
-export([broadcast_message/1
         ,direct_message/1
        ]).
-export([vote/1
        ,votes_enabled/1
        ,votes_visibility/1
        ,votes_reset/1
        ]).
-export([load_ticket/1
        ,configure_tickets/1
        ,load_tickets/1
        ,set_story_points/1
        ]).
-export([tick/1]).

-export([create_frame/3]).
-export([create_event/2]).
-export([not_implemented/0]).

-spec initial(aopp_participant:participant()) -> {'text', binary()}.
initial(Myself) ->
    Data = #{<<"ticket">> => maybe_initial_ticket()
            ,<<"myself">> => aopp_participant:to_map(Myself)
            ,<<"votes">> => aopp_votes:summarize()
            ,<<"participants">> =>
                 [aopp_participant:to_map(Participant)
                  || Participant <- aopp_participants:list()
                 ]
            },
    {'text', create_event(<<"initial">>, Data)}.

-spec maybe_initial_ticket() -> 'undefined' | map().
maybe_initial_ticket() ->
    case aopp_tickets:current() of
        'undefined' -> 'undefined';
         Ticket -> aopp_ticket:to_map(Ticket)
    end.

-spec update_name(map()) -> 'ok'.
update_name(Data) ->
    {'ok', Name} = maps:find(<<"name">>, Data),
    {'ok', Participant} = aopp_participants:find(self()),
    aopp_participants:update(
      aopp_participant:set_name(Name, Participant)
     ).

-spec promote(map()) -> 'ok'.
promote(_Data) ->
    {'ok', Participant} = aopp_participants:find(self()),
    aopp_participants:update(
      aopp_participant:set_moderator('true', Participant)
     ).

-spec broadcast_message(map()) -> 'ok' | 'error'.
broadcast_message(Data) ->
    {'ok', Message} = maps:find(<<"message">>, Data),
    aopp_participants:broadcast_message(self(), Message).

-spec direct_message(map()) -> 'ok' | 'error'.
direct_message(Data) ->
    {'ok', Id} = maps:find(<<"id">>, Data),
    {'ok', Message} = maps:find(<<"message">>, Data),
    aopp_participants:direct_message(Id, self(), Message).

-spec vote(map()) -> 'ok' | {'text', binary()}.
vote(Data) ->
    {'ok', Value} = maps:find(<<"value">>, Data),
    {'ok', Participant} = aopp_participants:find(self()),
    case aopp_votes:update(Participant, Value) of
        'disabled' ->
            Message = <<"Voting is currently disabled">>,
            {'text', create_event(<<"error">>, #{<<"message">> => Message})};
        _Else -> 'ok'
    end.

-spec votes_enabled(map()) -> 'ok'.
votes_enabled(Data) ->
    case maps:find(<<"enable">>, Data) of
        {'ok', 'true'} -> aopp_votes:enable();
        {'ok', 'false'} -> aopp_votes:disable()
    end.

-spec votes_visibility(map()) -> 'ok'.
votes_visibility(Data) ->
    case maps:find(<<"visible">>, Data) of
        {'ok', 'true'} -> aopp_votes:show_values();
        {'ok', 'false'} -> aopp_votes:hide_values()
    end.

-spec votes_reset(map()) -> 'ok'.
votes_reset(_) ->
    aopp_votes:reset().

-spec configure_tickets(map()) -> {'text', binary()} | [{'text', binary()}].
configure_tickets(Data) ->
    {'ok', Username} = maps:find(<<"username">>, Data),
    {'ok', Password} = maps:find(<<"password">>, Data),
    {'ok', Url} = maps:find(<<"base_url">>, Data),
    _ = aopp_tickets:set_base_url(Url),
    _ = aopp_tickets:set_credentials(Username, Password),
    case aopp_tickets:test_credentials() of
        {'ok', Projects} ->
            {'text', create_event(<<"jira_logged_in">>, Projects)};
        {'errors', Errors} ->
            [{'text', create_event(<<"error">>, #{<<"message">> => Error})}
             || Error <- Errors
            ];
        {'error', Error} ->
            {'text', create_event(<<"error">>, #{<<"message">> => Error})}
    end.

-spec load_tickets(map()) -> {'text', binary()} | [{'text', binary()}].
load_tickets(Data) ->
    {'ok', JQL} = maps:find(<<"jql">>, Data),
    case aopp_tickets:load(JQL) of
        {'ok', Tickets} ->
            {'text',
             create_event(
               <<"tickets_loaded">>,
               [aopp_ticket:to_map(Ticket)
                || Ticket <- Tickets
               ]
              )
            };
        {'errors', Errors} ->
            [{'text', create_event(<<"error">>, #{<<"message">> => Error})}
             || Error <- Errors
            ];
        {'error', Error} ->
            {'text', create_event(<<"error">>, #{<<"message">> => Error})}
        end.

-spec load_ticket(map()) -> 'ok' | {'text', binary()} | [{'text', binary()}].
load_ticket(Data) ->
    {'ok', Key} = maps:find(<<"key">>, Data),
    case aopp_tickets:jump_to(Key) of
        {'ok', _} -> 'ok';
        {'errors', Errors} ->
            [{'text', create_event(<<"error">>, #{<<"message">> => Error})}
             || Error <- Errors
            ];
        {'error', Error} ->
            {'text', create_event(<<"error">>, #{<<"message">> => Error})}
        end.

-spec tick(map()) -> 'ok'.
tick(Data) ->
    aopp_participants:broadcast_event(<<"tick">>, Data).

-spec set_story_points(map()) -> 'ok'.
set_story_points(Data) ->
    {'ok', Points} = maps:find(<<"story_points">>, Data),
    aopp_tickets:set_story_points(Points).

-spec create_frame(aopp_participant:participant() | 'undefined', binary(), _) -> {'text', binary()}.
create_frame('undefined', Event, Data) ->
    {'text', create_event(Event, Data)};
create_frame(From, Event, Message) ->
    Data = maps:put(
             <<"message">>
             ,Message
             ,aopp_participant:to_map(From)
            ),
    {'text', create_event(Event, Data)}.

-spec create_event(binary(), _) -> binary().
create_event(Event, Data) ->
    JObj = #{<<"event">> => Event
             ,<<"data">> => Data
            },
    jiffy:encode(JObj).

-spec not_implemented() -> {'text', binary()}.
not_implemented() ->
    {'text',
     create_event(
       <<"error">>,
       #{<<"message">> => <<"Request is not implemented">>}
      )
    }.
