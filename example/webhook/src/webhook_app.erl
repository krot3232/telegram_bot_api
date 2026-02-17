%%%-------------------------------------------------------------------
%% @doc webhook public API
%% @end
%%%-------------------------------------------------------------------

-module(webhook_app).

-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
	logger:set_application_level(ssl, warning),
	%% WebhookWhiteIP = <<"1.2.3.4">>
	%% You must use a public IP address. The function obtains the IP address via http://ifconfig.me/ip
	{ok,WebhookWhiteIP} = telegram_bot_api_util:get_ip(),
	%% Ports currently supported for webhooks: 443, 80, 88, 8443.
	WebhookPort=88,
	WebhookPort1=integer_to_binary(WebhookPort),
	%% Create bot use @BotFather, token ex: <<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>
	Token=list_to_binary(os:getenv("TELEGRAM_TOKEN1")),
	BotName1= pool_mybot1,
	%%The secret token must be the same for all bots running on this IP address and port.
	WebhookSecretToken= list_to_binary(os:getenv("TELEGRAM_SECRET","secret_token123")),
	io:format("~nToken: ~p~n~n",[Token]),

	%% Generating a certificate pair (PEM)
	%% https://core.telegram.org/bots/self-signed
	%% openssl req -newkey rsa:2048 -sha256 -nodes -keyout YOURPRIVATE.key -x509 -days 36500 -out YOURPUBLIC.pem -subj "/C=US/ST=New York/L=Brooklyn/O=Example Brooklyn Company/CN=WebhookWhiteIP"
	Certfile= <<"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem">>,
	Keyfile= <<"/etc/telegram_bot_api/ssl/YOURPRIVATE.key">>,

	Bot1 = #{name=>BotName1,token=>Token},
	BotName1Bin=atom_to_binary(BotName1), 
 
	BotEvent1 = binary_to_atom(list_to_binary(io_lib:format("webhook_event_AAA_~p", [BotName1]))),

	io:format("Bot: ~p event: ~p~n~n",[Bot1,BotEvent1]),
	%% 1. Start supervisor, start gen_event
	{ok,Pid}=webhook_sup:start_link([BotEvent1]),
	
	% 2. Add handler event bot1
	gen_event:add_handler({global,BotEvent1}, webhook_event_msg, [Bot1]),

	% 3. Create HTTP pool
    {ok, _Pid1} = telegram_bot_api_sup:start_pool(Bot1#{
        workers=>1
		,http_timeout=>4_000 %% or infinity 
		%,http_proxy=>{"127.0.0.1",8118}
		%http_endpoint=><<"https://api.telegram.org">>
      }),
	% http_timeout It can be more than 5 seconds only for asynchronous requests, otherwise there will be an error,
		% exit {timeout,{gen_server,call,
        %                        ['wpool_pool-pool_mybot1-1',
        %                         {raw,<<"editMessageText">>,
        %                              #{text => <<"40">>,message_id => 654,
        %                                chat_id => 123},
        %                              false},
        %                         5000]}}
	%%You must use async requests or pass a custom timeout when calling methods
	%%Example: telegram_bot_api:sendMessage(BotName,#{ chat_id=>ChatId, text=><<"text">> },false,infinity).
	%%You can request workers wpool:get_workers(BotName1)
	%%Each worker is a separate httpc profile; you can get all profiles like this: inets:services_info()
	%%httpc:get_options(all, 'wpool_pool-pool_mybot1-2').

	% 4. Create Rest Api Telegram webhook
	WebhookServer=telegram_bot_api_webhook_server:name_server(WebhookWhiteIP,WebhookPort),
	io:format("~n~nWebhookServer: ~p~n",[WebhookServer]),
	WebhookResult=telegram_bot_api_sup:start_webhook(#{
								id=>WebhookServer,
								secret_token=>WebhookSecretToken,
								%bots=>#{
									%%You can add all bots at the start_webhook creation stage, or dynamically after -> 4.1
									%% add 1 bot
									% BotName1Bin=>#{
									% 			event=>{global,BotEvent1},
									% 			name=>BotName1 	%%name atom
									% 		}
									% 	%%.. other bot
								%},
								%%ranch_ssl:opts(),
								transport_opts=>#{
									ip=>{0,0,0,0},
									port=>WebhookPort,
									%%If you use https, certificates are required.
									certfile=>Certfile,
									keyfile=>Keyfile,
									verify=> verify_none,
									versions=> proplists:get_value(supported,ssl:versions()),
									fail_if_no_peer_cert=>false,
									log_level=>none, %logger:level() | none | all
									%next_protocols_advertised=> [<<"h2">>, <<"http/1.1">>],
									%alpn_preferred_protocols=>[<<"h2">>, <<"http/1.1">>],
									keepalive=>true,
									nodelay=>true
								}
							}),
    %   {ok,_WebhookPid}=case WebhookResult of
    %         {error, {already_started, PidWh}} -> {ok, PidWh};
    %         PidWh -> PidWh
    %   end, 
	io:format("WebhookResult: ~p~n~n",[WebhookResult]),
	% 4.1 Add bot dinamic 
	% If you run multiple bots from different applications, you need to add them dynamically.
	WebhookAddBot= telegram_bot_api_webhook_server:add_bot(
			{global,WebhookServer},%|| WebhookPid
			BotName1Bin,
			#{
					event=>{global,BotEvent1},
					name=>BotName1 	%%name atom
			}
	),
	%%5. setWebhook (if not previously installed)
	WebhookUrl= telegram_bot_api_webhook_server:make_url(WebhookWhiteIP,WebhookPort1,BotName1Bin),
	try
	%%{ok,200,Result}
	Result=telegram_bot_api:setWebhook(BotName1,#{
		url=>WebhookUrl,
		ip_address=>WebhookWhiteIP,
		certificate=>#{
                 file=>Certfile,
                 name=><<"YOURPUBLIC.pem">>
                },
		%allowed_updates=>[message,callback_query,channel_post,message_reaction,message_reaction_count],%%see telegram_bot_api:'update_type'()
		secret_token=>WebhookSecretToken
	}),
	io:format("\e[0;102mResult setWebhook ~p~n\e[0m",[Result]),
	ok
	catch
		E:M:S->
			io:format("Result setWebhook Error ~p + ~p + ~p~n",[E,M,S]),
			error
	end,
	WebhookInfo=telegram_bot_api:getWebhookInfo(BotName1,#{}),
	io:format("\e[0;104mWebhook Add Bot: ~p Info: ~0p SecretToken: ~p~n~n\e[0m",[WebhookAddBot,WebhookInfo,WebhookSecretToken]),
	
	%io:format("~n~n\e[0;103mglobal: ~p~n\e[0m",[ets:tab2list(global_pid_names)]),
	% D=telegram_bot_api_webhook_server:delete_bot({global,WebhookServer},BotName1Bin),
	% io:format("~n~n\e[0;103mdel: ~p~n\e[0m",[D]),

	init_systemd_notify(),
	State = #{bots=>[
					{{global,WebhookServer},BotName1Bin}
					]
					},
	{ok,Pid, State}.
%%application:stop(webhook),
stop(#{bots:=Bots}=_State) ->
	[telegram_bot_api_webhook_server:delete_bot(Server,BotName)||{Server,BotName}<-Bots],
	systemd:notify(stopping),
    ok;
stop(_State) ->
	io:format("Stop ~p~n",[_State]),
	systemd:notify(stopping),
    ok.

%% systemd -> set status to ready and start watchdog
init_systemd_notify() ->
    Pid = os:getpid(),
    systemd:notify(ready),
    case os:getenv("WATCHDOG_PID") of
        false -> systemd:watchdog(enable);
        Pid -> systemd:watchdog(enable);
        _ -> false
    end.