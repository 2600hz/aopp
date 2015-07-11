-module(aopp_participant).

-export([new/1]).
-export([to_map/1]).
-export([id/1
         ,binary_id/1
        ]).
-export([pid/1]).
-export([monitor/1
         ,set_monitor/2
        ]).
-export([name/1
        ,set_name/2
        ]).
-export([moderator/1
         ,set_moderator/2
        ]).

-record(participant, {id :: pos_integer()
                      ,pid :: pid()
                      ,monitor :: reference() | 'undefined'
                      ,name = <<"unknown">> :: binary()
                      ,moderator = 'false' :: boolean()
                     }).
-type participant() :: #participant{}.
-export_type([participant/0]).

-spec new(pid()) -> participant().
new(Pid) when is_pid(Pid) ->
    #participant{
       id = erlang:unique_integer(['positive']),
       pid = Pid
      }.

-spec to_map(participant()) -> map().
to_map(#participant{}=Participant) ->
    #{<<"id">> => binary_id(Participant)
     ,<<"name">> => name(Participant)
     ,<<"moderator">> => moderator(Participant)
     }.

-spec id(participant()) -> pos_integer().
id(#participant{id=Id}) ->
    Id.

-spec binary_id(participant()) -> binary().
binary_id(Participant) ->
    integer_to_binary(id(Participant)).

-spec pid(participant()) -> pid().
pid(#participant{pid=Pid}) ->
    Pid.

-spec monitor(participant()) -> reference() | 'undefined'.
monitor(#participant{monitor=Monitor}) ->
    Monitor.

-spec set_monitor(reference(), participant()) -> participant().
set_monitor(Ref, #participant{}=Participant) when is_reference(Ref) ->
    Participant#participant{monitor=Ref}.

-spec name(participant()) -> binary().
name(#participant{name=Name}) ->
    Name.

-spec set_name(binary(), participant()) -> participant().
set_name(Name, #participant{}=Participant) ->
    Participant#participant{name=Name}.

-spec moderator(participant()) -> boolean().
moderator(#participant{moderator=Moderator}) ->
    Moderator.

-spec set_moderator(boolean(), participant()) -> participant().
set_moderator(Moderator, #participant{}=Participant) ->
    Participant#participant{moderator=Moderator}.
