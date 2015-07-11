-module(aopp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    _ = ssl:start(),
    _ = inets:start(),
    Dispatch =
        cowboy_router:compile(
          [
           {'_', [
                  {"/", 'cowboy_static', {'priv_file', 'aopp', "index.html"}},
                  {"/admin", 'cowboy_static', {'priv_file', 'aopp', "admin.html"}},
                  {"/websocket", 'aopp_handler', []},
                  {"/assets/[...]", 'cowboy_static', {'priv_dir', 'aopp', "assets"}}
                 ]}
          ]),
    {'ok', _} =
        cowboy:start_http('http', 100, [{'port', 8080}],
                          [{'env', [{'dispatch', Dispatch}]}
                          ]),
    aopp_sup:start_link().

stop(_State) ->
    'ok'.
