%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_webhook_server).
-behaviour(gen_server).

-moduledoc """
The server that runs the cowboy rest api to receive webhook data.

Allows you to add or remove new handlers for bots.

See `m:telegram_bot_api#webhook`
""".

-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([name_server/2]).
-export([add_bot/3, delete_bot/2]).
-export([make_url/3, make_url/4]).

-export([child_spec/1]).

-define(URL_PREF1, <<"telegram">>).
-define(URL_PREF2, <<"update">>).

-define(SCHEME, <<"https">>).

-export_type([bots/0, https/0]).

name_server({_, _, _, _} = Ip, Port) when is_integer(Port) ->
    IpBin = list_to_binary(inet:ntoa(Ip)),
    PortBin = integer_to_binary(Port),
    binary_to_atom(<<"webhook_", IpBin/binary, ":", PortBin/binary>>);
name_server(Ip, Port) when is_binary(Ip), is_binary(Port) ->
    binary_to_atom(<<"webhook_", Ip/binary, ":", Port/binary>>);
name_server(Ip, Port) ->
    binary_to_atom(list_to_binary(io_lib:format("webhook_~p:~p", [Ip, Port]))).

-type webhook_pid() :: atom() | {atom(), node()} | {global, term()} | {via, atom(), term()} | pid().

-type ipv4() :: {0..255, 0..255, 0..255, 0..255}.
-doc """
## Example:
```erlang
        #{
        ip=>{0,0,0,0},
        port=>8443,
        certfile=><<"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem">>,
        keyfile=><<"/etc/telegram_bot_api/ssl/YOURPRIVATE.key">>,
        verify=> verify_none
        }
```
""".
-type https() :: #{
    ip := ipv4(),
    port := pos_integer(),
    certfile := binary(),
    keyfile := binary(),
    verify => term(),
    _ => term()
}.

-type webhook_botname() :: binary().
-type webhook_botname_param() :: atom() | telegram_bot_api:pool_name().
-doc """
## Example:
```erlang
            #{
                <<"webhook_botname1">>=>#{
                    event=>{global,gen_event_server_bot1},
                    name=>bot_pool1
                },
                <<"bot2">>=>#{
                    event=>{global,gen_event_server_bot2},
                    name=>bot_pool2
                },
            }
```
""".
-type bots() :: #{webhook_botname() := bot()}.
-type bot() :: #{event := telegram_bot_api_app:event(), name := webhook_botname_param()}.

-spec start_link(
    Opts :: #{
        id := term(),
        bots := bots(),
        secret_token := WebhookSecretToken :: telegram_bot_api:secret_token(),
        https := https()
    }
) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(#{id := Id} = Opts) ->
    gen_server:start_link({global, Id}, ?MODULE, [Opts], []).
stop(Pid) ->
    gen_server:call(Pid, stop).

init([
    #{
        id := Id,
        bots := Bots,
        secret_token := SecretToken,
        https := #{ip := _Ip, port := _Port, certfile := _Certfile, keyfile := _Keyfile} =
            TransportOpts
    } = Opts
]) ->
    Dispatch = router_compile(Bots, SecretToken),
    {ok, ListenerPid} = cowboy:start_tls(
        Id,
        maps:to_list(TransportOpts),
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Opts#{listener_pid => ListenerPid}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(
    {add_bot, Name, Bot}, _From, #{id := Ref, secret_token := SecretToken, bots := Bots} = State
) ->
    BotsNew = Bots#{Name => Bot},
    Dispatch = router_compile(BotsNew, SecretToken),
    cowboy:set_env(Ref, dispatch, Dispatch),
    {reply, ok, State#{bots => BotsNew}};
handle_call(
    {delete_bot, Name}, _From, #{id := Ref, secret_token := SecretToken, bots := Bots} = State
) ->
    BotsNew = maps:remove(Name, Bots),
    Dispatch = router_compile(BotsNew, SecretToken),
    cowboy:set_env(Ref, dispatch, Dispatch),
    {reply, ok, State#{bots => BotsNew}};
handle_call({set_secret_token, SecretToken}, _From, State) ->
    {reply, ok, State#{set_secret_token => SecretToken}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{id := Id} = _State) ->
    cowboy:stop_listener(Id),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%
-doc """
Adds a new bot to the webhook api
## Example:
```erlang
    %%URL=https://myip:myport/telegram/webhook_botname123/update
    ok= telegram_bot_api_webhook_server:add_bot({global,WebhookServer},<<"webhook_botname123">
     		#{
     				event=>{global,BotEvent},
     				name=>bot1
     		}
     )
```
""".
-spec add_bot(Pid :: webhook_pid(), Name :: webhook_botname(), Bot :: bot()) -> ok | term().
add_bot(Pid, Name, Bot) ->
    gen_server:call(Pid, {add_bot, Name, Bot}).
-spec delete_bot(Pid :: webhook_pid(), Name :: webhook_botname()) -> ok | term().
delete_bot(Pid, Name) ->
    gen_server:call(Pid, {delete_bot, Name}).
%%%%%%%%%%%%%%%%%%%%%%%%
router_compile(Bots, SecretToken) ->
    cowboy_router:compile([
        {'_', [
            {
                <<$/, ?URL_PREF1/binary, $/, "[:bot_name]", $/, ?URL_PREF2/binary>>,
                telegram_bot_api_webhook_update_h,
                #{bots => Bots, secret_token => SecretToken}
            },
            {'_', telegram_bot_api_webhook_error_h, []}
        ]}
    ]).
make_url(Ip, Port, BotName) -> make_url(?SCHEME, Ip, Port, BotName).
make_url(Scheme, Ip, Port, BotName) ->
    <<Scheme/binary, "://", Ip/binary, $:, Port/binary, $/, ?URL_PREF1/binary, $/, BotName/binary,
        $/, ?URL_PREF2/binary>>.

child_spec(#{secret_token := _, bots := _, https := #{ip := Ip, port := Port}} = Op) ->
    Id =
        case maps:get(id, Op, undef) of
            Id1 when is_atom(Id1), Id1 =/= undef -> Id1;
            _ -> name_server(Ip, Port)
        end,
    #{
        id => Id,
        start => {telegram_bot_api_webhook_server, start_link, [Op#{id => Id}]},
        restart => transient,
        shutdown => brutal_kill,
        type => worker,
        modules => [telegram_bot_api_webhook_server]
    }.
