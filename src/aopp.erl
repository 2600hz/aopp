-module(aopp).

-export([start/0
         ,start/1
        ]).

start() ->
    application:ensure_all_started('aopp').

start([App]) ->
    application:ensure_all_started(App).
