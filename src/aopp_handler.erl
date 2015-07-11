-module(aopp_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({_Transport, _Protocol}, _Req, _Opts) ->
    {'upgrade', 'protocol', 'cowboy_websocket'}.

websocket_init(_Transport, Req, State) ->
    Participant = aopp_participant:new(self()),
    _ = aopp_participants:add(Participant),
    self() ! {'initialize', Participant},
    {'ok', Req, State}.

websocket_handle({'text', Text}, Req, State) ->
    case handle_text_message(Text) of
        'ok' -> {'ok', Req, State};
        Reply -> {'reply', Reply, Req, State}
    end;
websocket_handle(_InFrame, Req, State) ->
    {'ok', Req, State}.

websocket_info({'send', From, {Event, Data}}, Req, State) ->
    {'reply', aopp_api:create_frame(From, Event, Data), Req, State};
websocket_info({'initialize', Participant}, Req, State) ->
    {'reply',aopp_api:initial(Participant), Req, State};
websocket_info(_Info, Req, State) ->
    {'ok', Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    'ok'.

-spec handle_text_message(binary()) -> 'undefined' | {'text', binary()} | [{'text', binary()}].
handle_text_message(Text) ->
    try jiffy:decode(Text, ['return_maps']) of
        JObj ->
            {'ok', Event} = maps:find(<<"event">>, JObj),
            {'ok', Data} = maps:find(<<"data">>, JObj),
            maybe_execute_api(Event, Data)
    catch
        _:_ -> aopp_api:not_implemented()
    end.

-spec maybe_execute_api(atom() | binary(), map()) -> 'undefined' | {'text', binary()} | [{'text', binary()}].
maybe_execute_api(Event, Data) when is_binary(Event) ->
    try binary_to_existing_atom(Event, 'latin1') of
        Function -> maybe_execute_api(Function, Data)
    catch
        _:_ -> aopp_api:not_implemented()
    end;
maybe_execute_api(Function, Data) ->
    case erlang:function_exported('aopp_api', Function, 1) of
        'false' -> aopp_api:not_implemented();
        'true' -> execute_api(Function, Data)
    end.

-spec execute_api(atom(), map()) -> 'undefined' | {'text', binary()} | [{'text', binary()}].
execute_api(Function, Data) ->
    erlang:apply('aopp_api', Function, [Data]).
