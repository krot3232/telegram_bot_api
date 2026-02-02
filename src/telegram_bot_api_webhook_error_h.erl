%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_webhook_error_h).
-behaviour(cowboy_handler).

-moduledoc """
REST API handler for page not found

See `m:telegram_bot_api#webhook`
""".

-export([init/2]).
init(Req0, Opts) ->
    Req = cowboy_req:reply(
        404,
        #{<<"server">> => <<"none">>},
        <<"404">>,
        Req0
    ),
    {ok, Req, Opts}.
