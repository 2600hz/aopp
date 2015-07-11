-module(aopp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = sup_flags('one_for_one', 5, 10),
    ChildSpecs =
        [child_spec('participants', 'aopp_participants', 'start')
        ,child_spec('tickets', 'aopp_tickets', 'start')
        ,child_spec('votes', 'aopp_votes', 'start')
        ],
    {'ok', {SupFlags, ChildSpecs}}.

-spec sup_flags(supervisor:strategy(), non_neg_integer(), pos_integer()) -> supervisor:sup_flags().
sup_flags(Strategy, Intensity, Period) ->
    {Strategy, Intensity, Period}.

-spec child_spec(supervisor:child_id(), supervisor:module(), atom()) -> _.
child_spec(Id, M, F) ->
    child_spec(Id, M, F, []).

-spec child_spec(supervisor:child_id(), supervisor:module(), atom(), [term()] | 'undefined') -> supervisor:child_spec().
child_spec(Id, M, F, A) ->
    child_spec(Id, {M, F, A}, 'permanent', 5000, 'worker', [M]).

-spec child_spec(supervisor:child_id(), supervisor:mfargs(), supervisor:restart(), supervisor:shutdown(), supervisor:type(), supervisor:modules()) -> supervisor:child_spec().
child_spec(Id, Start, Restart, Shutdown, Type, Modules) ->
    {Id, Start, Restart, Shutdown, Type, Modules}.
