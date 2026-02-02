%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_webhook_update_h).
-behaviour(cowboy_rest).

-moduledoc """
REST API handler for webhook.
A page that receives data from Telegram and sends it to a webhook installed via setWebhook.
URL: https://[ip]:[port]/telegram/[bot_name]/update

See `m:telegram_bot_api#webhook`
""".

-export([init/2, allowed_methods/2, content_types_accepted/2, resource_exists/2]).
-export([is_authorized/2]).
-export([handle_post/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{'*', handle_post}], Req, State}.

resource_exists(Req, #{bots := Bots} = State) ->
    Bot = cowboy_req:binding(bot_name, Req),
    case maps:get(Bot, Bots, undefined) of
        undefined -> {false, Req, State};
        V -> {true, Req, State#{bot => V}}
    end.

is_authorized(Req, #{secret_token := SecretToken} = State) ->
    Token = cowboy_req:header(<<"x-telegram-bot-api-secret-token">>, Req),
    if
        Token =:= SecretToken -> {true, Req, State};
        true -> {{false, <<"Basic realm=\"secret_token\"">>}, Req, State}
    end.

handle_post(Req0, #{bot := #{event := Event, name := Name}} = State) ->
    try
        {ok, Data, _} = cowboy_req:read_body(Req0),
        Json = telegram_bot_api_util:json_decode(Data),
        ok = gen_event:notify(Event, {update, Name, Json}),
        {true, Req0, State}
    catch
        E:M ->
            ok = gen_event:notify(Event, {error, Name, E, M}),
            {false, Req0, State}
    end;
handle_post(Req0, State) ->
    {false, Req0, State}.
