%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_app).
-behaviour(application).

-export([start/2, stop/1]).

-export_type([event/0]).

-type event() :: atom() | {atom(), node()} | {global, term()} | {via, atom(), term()} | pid().

start(_StartType, _StartArgs) ->
    telegram_bot_api_sup:start_link().
stop(_State) ->
    ok.
