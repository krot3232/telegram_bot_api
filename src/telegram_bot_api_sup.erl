-module(telegram_bot_api_sup).

-behaviour(supervisor).

-moduledoc """
telegram_bot_api top level supervisor.
""".

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-export([start_pool/1, start_update/1, start_webhook/1]).

-type child_id() :: term().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    SupFlags = #{
        strategy => application:get_env(telegram_bot_api, supervisor_strategy, one_for_one),
        intensity => application:get_env(telegram_bot_api, supervisor_intensity, 1000),
        period => application:get_env(telegram_bot_api, supervisor_period, 60)
    },
    {ok, {SupFlags, []}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
Start Http pool telegram bot
### Parameters:
- `name` - Unique http pool name, separate pool for each bot!
- `token` - Bot token obtained from [@BotFather](https://t.me/BotFather), example: `1234556678:ABNFSERRTYUERYTYUurVGhgFrtyReWuthTW`
- `workers` - Count of workers, The worker is a module `m:telegram_bot_api_server`
- `http_module` - Module for implementing HTTP requests, by default `m:telegram_bot_api_http`
- `http_endpoint` - Url telegram api, by default https://api.telegram.org
- `http_option` -  HttpOption [httpc:request](https://www.erlang.org/doc/apps/inets/httpc.html#request/5), by default `[{timeout,10000}, {ssl, [{verify, verify_none}]}]`
- `http_timeout` - Http timeout, by default `10000`
- `option_request` - OptionRequest for [httpc:request](https://www.erlang.org/doc/apps/inets/httpc.html#request/5)
## Examples:
```erlang
     {ok, Pid} = telegram_bot_api_sup:start_pool(#{
       name=>pool_name_bot1,
       token=><<"1000000000:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ">>
       workers=>1
 }).
```
""".
-spec start_pool(#{
    name := telegram_bot_api:pool_name(),
    token := binary(),
    workers := integer(),
    http_module => telegram_bot_api_http:module_name(),
    http_endpoint => telegram_bot_api_http:http_endpoint(),
    http_option => telegram_bot_api_http:http_option(),
    http_timeout => telegram_bot_api_http:http_timeout(),
    option_request => telegram_bot_api_http:option_request(),
    http_proxy => telegram_bot_api_http:http_proxy()
}) -> supervisor:startchild_ret() | supervisor:startchild_err() | {error, term()}.

start_pool(#{name := Pool, token := _, workers := Workers} = Op) ->
    try
        ChildSpec = wpool:child_spec(Pool, [
            {workers, Workers},
            {worker, {telegram_bot_api_server, [Op]}},
            {work_type, gen_server}
        ]),
        supervisor:start_child(?SERVER, ChildSpec)
    catch
        E:M -> {error, {E, M}}
    end.

-doc """
Start receive incoming updates using long polling
### Parameters:
- `name` - Unique http pool name, separate pool for each bot!
## Examples:
```erlang
     {ok, Pid} = telegram_bot_api_sup:start_update(#{
               name=>bot1,
               update_time=>1000,
               offset=>0,
               limit=>100,
               allowed_updates=>[message,callback_query,channel_post],
               event=>server_event
         }).
```
""".
-spec start_update(
    Op :: #{
        id => child_id(),
        name := Pool :: telegram_bot_api:pool_name(),
        update_time := UpdateTime :: integer(),
        offset := Offset :: integer(),
        event := Event :: telegram_bot_api_app:event(),
        limit => Limit :: integer(),
        allowed_updates => AllowedUpdates :: nonempty_list(telegram_bot_api:update_type())
    }
) -> supervisor:startchild_ret() | supervisor:startchild_err().
start_update(
    #{
        name := _Pool,
        update_time := _UpdateTime,
        offset := _Offset,
        event := _E
    } = Op
) ->
    try
        ChildSpec = telegram_bot_api_updater_server:child_spec(Op),
        supervisor:start_child(?SERVER, ChildSpec)
    catch
        E:M -> {error, {E, M}}
    end.

-doc """
Start webhook
### Parameters:
- `secret_token` - A secret token to be sent in a header “X-Telegram-Bot-Api-Secret-Token” in every webhook request, 1-256 characters.
- [`bots`](`t:telegram_bot_api_webhook_server:bots/0`) - Maps bot
- [`https`](`t:telegram_bot_api_webhook_server:https/0`) - Maps contains the IP address and port for REST API webhook, use for set url webhook `telegram_bot_api:setWebhook/3`
## Examples:
```erlang
telegram_bot_api_sup:start_webhook(#{
                    id=>server_webhook,
                    secret_token=><<"WebhookSecretToken">>,
                    bots=>#{
                        <<"bot1">>=>#{
                                    event=>{global,bot_event_server1},
                                    name=>bot1
                                }
                            %%.. other bot
                    },
                    https=>#{
                        ip=>{0,0,0,0},
                        port=>8443,
                        certfile=>Certfile,
                        keyfile=>Keyfile,
                        verify=> verify_none
                    }
                    }).
```
""".
-spec start_webhook(
    Op :: #{
        id => child_id(),
        secret_token := telegram_bot_api:secret_token(),
        bots := telegram_bot_api_webhook_server:bots(),
        https := telegram_bot_api_webhook_server:https()
    }
) -> supervisor:startchild_ret() | supervisor:startchild_err().
start_webhook(#{secret_token := _, bots := _, https := #{ip := _Ip, port := _Port}} = Op) ->
    try
        ChildSpec = telegram_bot_api_webhook_server:child_spec(Op),
        supervisor:start_child(?SERVER, ChildSpec)
    catch
        E:M -> {error, {E, M}}
    end.
