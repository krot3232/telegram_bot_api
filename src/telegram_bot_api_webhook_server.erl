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
-export([get_bots/1]).

-export([child_spec/1]).
-export([get_port/1]).

-define(URL_PREF1, <<"telegram">>).
-define(URL_PREF2, <<"update">>).

-define(SCHEME, <<"https">>).

-export_type([bots/0, transport_opts/0, protocol_opts/0]).

name_server({_, _, _, _} = Ip, Port) when is_integer(Port) ->
    IpBin = list_to_binary(inet:ntoa(Ip)),
    PortBin = integer_to_binary(Port),
    binary_to_atom(<<"webhook_", IpBin/binary, ":", PortBin/binary>>);
name_server(Ip, Port) when is_binary(Ip), is_binary(Port) ->
    binary_to_atom(<<"webhook_", Ip/binary, ":", Port/binary>>);
name_server(Ip, Port) when is_binary(Ip), is_integer(Port) ->
    Port1 = integer_to_binary(Port),
    binary_to_atom(<<"webhook_", Ip/binary, ":", Port1/binary>>);
name_server(Ip, Port) when is_integer(Ip), is_binary(Port) ->
    Ip1 = integer_to_binary(Ip),
    binary_to_atom(<<"webhook_", Ip1/binary, ":", Port/binary>>);
name_server(Ip, Port) ->
    binary_to_atom(list_to_binary(io_lib:format("webhook_~p:~p", [Ip, Port]))).

-type webhook_pid() :: atom() | {atom(), node()} | {global, term()} | {via, atom(), term()} | pid().

-type log_level() :: logger:level() | none | all.

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

-type protocol_opts() :: http_opts() | http2_opts().

-type filename() :: binary() | string().

-type transport_opts() :: #{
    ip := inet:ip_address() | inet:local_address(),
    port := inet:port_number(),
    backlog => non_neg_integer(),
    buffer => non_neg_integer(),
    delay_send => boolean(),
    dontroute => boolean(),
    exit_on_close => boolean(),
    fd => non_neg_integer(),
    high_msgq_watermark => non_neg_integer(),
    high_watermark => non_neg_integer(),
    inet => true,
    inet6 => true,
    ipv6_v6only => boolean(),
    keepalive => boolean(),
    linger => boolean() | non_neg_integer(),
    low_msgq_watermark => non_neg_integer(),
    low_watermark => non_neg_integer(),
    nodelay => boolean(),
    priority => integer(),
    raw => non_neg_integer() | non_neg_integer() | binary(),
    recbuf => non_neg_integer(),
    send_timeout => timeout(),
    send_timeout_close => boolean(),
    sndbuf => non_neg_integer(),
    tos => integer(),
    alpn_preferred_protocols => [binary()],
    anti_replay => '10k' | '100k' | {integer(), integer(), integer()},
    beast_mitigation => one_n_minus_one | zero_n | disabled,
    cacertfile => filename(),
    cacerts => [public_key:der_encoded()],
    cert => public_key:der_encoded(),
    certs_keys => [
        #{
            cert => public_key:der_encoded(),
            key => ssl:key(),
            certfile => filename(),
            keyfile => filename(),
            key_pem_password => iodata() | fun(() -> iodata())
        }
    ],
    certfile => filename(),
    ciphers => ssl:ciphers(),
    client_renegotiation => boolean(),
    crl_cache => [any()],
    crl_check => boolean() | peer | best_effort,
    depth => integer(),
    dh => binary(),
    dhfile => filename(),
    eccs => [ssl:named_curve()],
    fail_if_no_peer_cert => boolean(),
    handshake => hello | full,
    hibernate_after => timeout(),
    honor_cipher_order => boolean(),
    honor_ecc_order => boolean(),
    key => ssl:key(),
    key_update_at => pos_integer(),
    keyfile => filename(),
    log_alert => boolean(),
    log_level => log_level(),
    max_handshake_size => integer(),
    middlebox_comp_mode => boolean(),
    next_protocols_advertised => [binary()],
    padding_check => boolean(),
    partial_chain => fun(),
    password => string(),
    protocol => tls | dtls,
    psk_identity => string(),
    reuse_session => fun(),
    reuse_sessions => boolean(),
    secure_renegotiate => boolean(),
    session_tickets => disabled | stateful | stateless,
    signature_algs => [{ssl:hash(), ssl:sign_algo()}],
    signature_algs_cert => [ssl:sign_scheme()],
    sni_fun => fun(),
    sni_hosts => [{string(), any()}],
    supported_groups => [ssl:group()],
    user_lookup_fun => {fun(), any()},
    verify => verify_none | verify_peer,
    verify_fun => {fun(), any()},
    versions => [ssl:protocol_version()],
    _ => _
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
-type bots() :: #{webhook_botname() := bot()} | #{}.
-type bot() :: #{event := telegram_bot_api_app:event(), name := webhook_botname_param()}.

-type http_opts() :: #{
    active_n => pos_integer(),
    alpn_default_protocol => http | http2,
    chunked => boolean(),
    compress_buffering => boolean(),
    compress_threshold => non_neg_integer(),
    connection_type => worker | supervisor,
    dynamic_buffer => false | {pos_integer(), pos_integer()},
    dynamic_buffer_initial_average => non_neg_integer(),
    dynamic_buffer_initial_size => pos_integer(),
    env => cowboy_middleware:env(),
    hibernate => boolean(),
    http10_keepalive => boolean(),
    idle_timeout => timeout(),
    inactivity_timeout => timeout(),
    initial_stream_flow_size => non_neg_integer(),
    linger_timeout => timeout(),
    logger => module(),
    max_authority_length => non_neg_integer(),
    max_empty_lines => non_neg_integer(),
    max_header_name_length => non_neg_integer(),
    max_header_value_length => non_neg_integer(),
    max_headers => non_neg_integer(),
    max_keepalive => non_neg_integer(),
    max_method_length => non_neg_integer(),
    max_request_line_length => non_neg_integer(),
    metrics_callback => cowboy_metrics_h:metrics_callback(),
    metrics_req_filter => fun((cowboy_req:req()) -> map()),
    metrics_resp_headers_filter => fun((cowboy:http_headers()) -> cowboy:http_headers()),
    middlewares => [module()],
    protocols => [http | http2],
    proxy_header => boolean(),
    request_timeout => timeout(),
    reset_idle_timeout_on_send => boolean(),
    sendfile => boolean(),
    shutdown_timeout => timeout(),
    stream_handlers => [module()],
    tracer_callback => cowboy_tracer_h:tracer_callback(),
    tracer_flags => [atom()],
    tracer_match_specs => cowboy_tracer_h:tracer_match_specs(),
    _ => _
}.

-type http2_opts() :: #{
    active_n => pos_integer(),
    alpn_default_protocol => http | http2,
    compress_buffering => boolean(),
    compress_threshold => non_neg_integer(),
    connection_type => worker | supervisor,
    connection_window_margin_size => 0..16#7fffffff,
    connection_window_update_threshold => 0..16#7fffffff,
    dynamic_buffer => false | {pos_integer(), pos_integer()},
    dynamic_buffer_initial_average => non_neg_integer(),
    dynamic_buffer_initial_size => pos_integer(),
    enable_connect_protocol => boolean(),
    env => cowboy_middleware:env(),
    goaway_initial_timeout => timeout(),
    goaway_complete_timeout => timeout(),
    hibernate => boolean(),
    idle_timeout => timeout(),
    inactivity_timeout => timeout(),
    initial_connection_window_size => 65535..16#7fffffff,
    initial_stream_window_size => 0..16#7fffffff,
    linger_timeout => timeout(),
    logger => module(),
    max_concurrent_streams => non_neg_integer() | infinity,
    max_connection_buffer_size => non_neg_integer(),
    max_connection_window_size => 0..16#7fffffff,
    max_decode_table_size => non_neg_integer(),
    max_encode_table_size => non_neg_integer(),
    max_fragmented_header_block_size => 16384..16#7fffffff,
    max_frame_size_received => 16384..16777215,
    max_frame_size_sent => 16384..16777215 | infinity,
    max_received_frame_rate => {pos_integer(), timeout()},
    max_reset_stream_rate => {pos_integer(), timeout()},
    max_cancel_stream_rate => {pos_integer(), timeout()},
    max_stream_buffer_size => non_neg_integer(),
    max_stream_window_size => 0..16#7fffffff,
    metrics_callback => cowboy_metrics_h:metrics_callback(),
    metrics_req_filter => fun((cowboy_req:req()) -> map()),
    metrics_resp_headers_filter => fun((cowboy:http_headers()) -> cowboy:http_headers()),
    middlewares => [module()],
    preface_timeout => timeout(),
    protocols => [http | http2],
    proxy_header => boolean(),
    reset_idle_timeout_on_send => boolean(),
    sendfile => boolean(),
    settings_timeout => timeout(),
    shutdown_timeout => timeout(),
    stream_handlers => [module()],
    stream_window_data_threshold => 0..16#7fffffff,
    stream_window_margin_size => 0..16#7fffffff,
    stream_window_update_threshold => 0..16#7fffffff,
    tracer_callback => cowboy_tracer_h:tracer_callback(),
    tracer_flags => [atom()],
    tracer_match_specs => cowboy_tracer_h:tracer_match_specs(),
    _ => _
}.

-spec start_link(
    Opts :: #{
        id := Id :: term(),
        bots => Boots :: bots(),
        secret_token := WebhookSecretToken :: telegram_bot_api:secret_token(),
        transport_opts := TransportOpts :: transport_opts(),
        protocol_opts => ProtocolOpts :: protocol_opts()
    }
) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(
    #{id := Id, secret_token := _SecretToken, transport_opts := #{ip := _, port := _}} = Opts
) ->
    gen_server:start_link({global, Id}, ?MODULE, [Opts], []).
stop(Pid) ->
    gen_server:call(Pid, stop).

init([
    #{
        id := RanchRef,
        secret_token := SecretToken,
        transport_opts := TransportOpts1
    } = Opts
]) ->
    Bots = maps:get(bots, Opts, #{}),
    TransportOpts = maps:to_list(TransportOpts1),
    DispatchHttps = router_compile(Bots, SecretToken),
    ProtocolOpts1 = maps:get(protocol_opts, Opts, #{}),
    ProtocolOpts = ProtocolOpts1#{env => #{dispatch => DispatchHttps}},
    {ok, _ListenerPid} = cowboy:start_tls(
        RanchRef,
        TransportOpts,
        ProtocolOpts
    ),
    Port1 = ranch:get_port(RanchRef),
    {ok, Opts#{port => Port1, bots => Bots}}.

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
handle_call(get_bots, _From, #{bots := Bots} = State) ->
    {reply, {ok, Bots}, State};
handle_call(get_port, _From, #{port := Port} = State) ->
    {reply, {ok, Port}, State};
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

-spec get_bots(Pid :: webhook_pid()) -> {ok, Boots :: bots()} | term().
get_bots(Pid) ->
    gen_server:call(Pid, get_bots).

-spec get_port(Pid :: webhook_pid()) -> {ok, integer()} | term().
get_port(Pid) ->
    gen_server:call(Pid, get_port).

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

child_spec(#{secret_token := _, transport_opts := #{ip := Ip, port := Port}} = Op) ->
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
