-module(aopp_participants).
-behaviour(gen_server).

-export([start/0]).
-export([list/0]).
-export([add/1]).
-export([find/1]).
-export([update/1]).
-export([count/0]).
-export([broadcast_event/2
         ,broadcast_message/2
        ]).
-export([direct_message/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {participants = dict:new() :: dict:dict()}).
-type state() :: #state{}.
-type participant_address() :: pos_integer() | pid() | aopp_participant:participant().

-spec start() -> {'ok', pid()} | 'ignore' | {'error', _}.
start() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec list() -> [aopp_participant:participant()].
list() ->
    gen_server:call(?MODULE, 'get_all').

-spec add(aopp_participant:participant()) -> 'ok'.
add(Participant) ->
    gen_server:cast(?MODULE, {'add', Participant}).

-spec find(pos_integer() | pid()) -> {'ok', aopp_participant:participant()} | 'error'.
find(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {'find_by_pid', Pid});
find(Id) when is_integer(Id) ->
    gen_server:call(?MODULE, {'find_by_id', Id}).

-spec update(aopp_participant:participant()) -> 'ok'.
update(Participant) ->
    gen_server:cast(?MODULE, {'update', Participant}).

-spec count() -> non_neg_integer().
count() ->
    gen_server:call(?MODULE, 'count').

-spec broadcast_event(binary(), _) -> 'ok'.
broadcast_event(Event, Data) ->
    send(list(), 'undefined', {Event, Data}).

-spec broadcast_message(participant_address(), _) -> 'ok' | 'error'.
broadcast_message(From, Message) when is_integer(From); is_pid(From) ->
    case find(From) of
        'error' -> 'error';
        {'ok', Participant} ->
            broadcast_message(Participant, Message)
    end;
broadcast_message(From, Message) ->
    send(list(), From, {<<"broadcast_message">>, Message}).

-spec direct_message(participant_address(), participant_address(), _) ->
                  'ok' | 'error'.
direct_message(To, From, Message) when is_integer(To); is_pid(To) ->
    case find(To) of
        'error' -> 'error';
        {'ok', Participant} ->
            direct_message(Participant, From, Message)
    end;
direct_message(To, From, Message) when is_integer(From); is_pid(From) ->
    case find(From) of
        'error' ->
            direct_message(To, 'undefined', Message);
        {'ok', Participant} ->
            direct_message(To, Participant, Message)
    end;
direct_message(To, From, Message) ->
    send(To, From, {<<"direct_message">>, Message}).

-spec init(_) -> {'ok', state()}.
init(_Args) ->
    {'ok', #state{}}.

-spec handle_call(_, {pid(), _}, state()) -> {'reply', _, state()}.
handle_call({'find_by_pid', Pid}, _From, State) ->
    {'reply', find_by_pid(Pid, State), State};
handle_call({'find_by_id', Id}, _From, State) ->
    {'reply', find_by_id(Id, State), State};
handle_call('count', _From, State) ->
    {'reply', count_participants(State), State};
handle_call('get_all', _From, State) ->
    {'reply', get_all_participants(State), State};
handle_call(_Request, _From, State) ->
    {'reply', 'not_implemented', State}.

-spec handle_cast(_, state()) -> {'noreply', state()}.
handle_cast({'add', Participant}, State) ->
    {'noreply', maybe_add_paticipant(Participant, State)};
handle_cast({'update', Participant}, State) ->
    {'noreply', maybe_update_paticipant(Participant, State)};
handle_cast(_Request, State) ->
    {'noreply', State}.

-spec handle_info(_, state()) -> {'noreply', state()}.
handle_info({'DOWN', _Ref, 'process', Pid, _Info}, State) ->
    {'noreply', remove_participant(Pid, State)};
handle_info(_Message, State) ->
    {'noreply', State}.

-spec terminate(_, state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

-spec code_change(_, state(), _) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_add_paticipant(aopp_participant:participant(), state()) -> state().
maybe_add_paticipant(Participant, #state{participants=Participants}=State) ->
    Pid = aopp_participant:pid(Participant),
    case dict:is_key(Pid, Participants) of
        'true' ->  State;
        'false' -> add_participant(Participant, State)
    end.

-spec add_participant(aopp_participant:participant(), state()) -> state().
add_participant(Participant, #state{participants=Participants}=State) ->
    _ = send(get_all_participants(State), Participant, {<<"new_participant">>, <<>>}),
%%    _ = send(Participant, Participant, {<<"connected">>, <<>>}),
    Pid = aopp_participant:pid(Participant),
    State#state{
      participants=dict:store(Pid, monitor_participant(Participant), Participants)
     }.

-spec monitor_participant(aopp_participant:participant()) -> aopp_participant:participant().
monitor_participant(Participant) ->
    Pid = aopp_participant:pid(Participant),
    MonitorRef = monitor('process', Pid),
    aopp_participant:set_monitor(MonitorRef, Participant).

-spec maybe_update_paticipant(aopp_participant:participant(), state()) -> state().
maybe_update_paticipant(Participant, #state{participants=Participants}=State) ->
    Pid = aopp_participant:pid(Participant),
    case dict:is_key(Pid, Participants) of
        'false' ->  State;
        'true' -> update_participant(Participant, State)
    end.

-spec update_participant(aopp_participant:participant(), state()) -> state().
update_participant(Participant, #state{participants=Participants}=State) ->
    _ = send(get_all_participants(State), Participant, {<<"update_participant">>, <<>>}),
    Pid = aopp_participant:pid(Participant),
    State#state{
      participants=dict:store(Pid, monitor_participant(Participant), Participants)
     }.

-spec remove_participant(pid(), state()) -> state().
remove_participant(Pid, #state{participants=Participants}=State) ->
    case find_by_pid(Pid, State) of
        'error' -> State;
        {'ok', Participant} ->
            _ = send(get_all_participants(State), Participant, {<<"remove_participant">>, <<>>}),
            _ = aopp_votes:remove(Participant),
            State#state{participants=dict:erase(Pid, Participants)}
    end.

-spec find_by_pid(pid(), state()) -> {'ok', aopp_participant:participant()} | 'error'.
find_by_pid(Pid, #state{participants=Participants}) ->
    dict:find(Pid, Participants).

-spec find_by_id(pos_integer(), state()) -> {'ok', aopp_participant:participant()} | 'error'.
find_by_id(Id, #state{participants=Participants}) ->
    dict:fold(fun(_, Participant, Acc) ->
                      case aopp_participant:id(Participant) =:= Id of
                          'true' -> Participant;
                          'false' -> Acc
                      end
              end, 'error', Participants).

-spec count_participants(state()) -> non_neg_integer().
count_participants(#state{participants=Participants}) ->
    dict:size(Participants).

-spec get_all_participants(state()) -> [aopp_participant:participant()].
get_all_participants(#state{participants=Participants}) ->
    [Value || {_, Value} <- dict:to_list(Participants)].

-type send_address() :: aopp_participant:participant() | [aopp_participant:participant()].
-spec send(send_address(), aopp_participant:participant() | 'undefined', _) -> 'ok'.
send([], _, _) -> 'ok';
send([To|Remaining], From, Message) ->
    _ = send(To, From, Message),
    send(Remaining, From, Message);
send(To, From, Message) ->
    _ = aopp_participant:pid(To) ! {'send', From, Message},
    'ok'.
