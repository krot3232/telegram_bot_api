%%%-------------------------------------------------------------------
%% @doc webhook public API
%% @end
%%%-------------------------------------------------------------------

-module(webhook_app).

-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
	%% WebhookWhiteIP = <<"1.2.3.4">> 
	{ok,WebhookWhiteIP} = telegram_bot_api_util:get_ip(),
	%% Ports currently supported for webhooks: 443, 80, 88, 8443.
	WebhookPort=8443,
	WebhookPort1=integer_to_binary(WebhookPort),
	%% Create bot use @BotFather
	Token= <<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
	BotName1= bot1,
	WebhookSecretToken= <<"secret_token123">>,

	%% Generating a certificate pair (PEM)
	%% https://core.telegram.org/bots/self-signed
	%% openssl req -newkey rsa:2048 -sha256 -nodes -keyout YOURPRIVATE.key -x509 -days 36500 -out YOURPUBLIC.pem -subj "/C=US/ST=New York/L=Brooklyn/O=Example Brooklyn Company/CN=WebhookWhiteIP"
	Certfile= <<"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem">>,
	Keyfile= <<"/etc/telegram_bot_api/ssl/YOURPRIVATE.key">>,

	Bot1 = #{name=>BotName1,token=>Token},
	BotName1Bin=atom_to_binary(BotName1), 
 
	BotEvent1 = binary_to_atom(list_to_binary(io_lib:format("webhook_event_AAA_~p", [BotName1]))),

	io:format("bot: ~p event: ~p~n",[Bot1,BotEvent1]),
	%% 1. Start supervisor, start gen_event
	{ok,Pid}=webhook_sup:start_link([BotEvent1]),
	

	% 2. Add handler event bot1
	gen_event:add_handler({global,BotEvent1}, webhook_event_msg, [Bot1]),

	% 3. Create HTTP pool
    {ok, _Pid1} = telegram_bot_api_sup:start_pool(Bot1#{
        workers=>1
		%http_endpoint=><<"https://api.telegram.org">>
      }),


	% 4. Create Rest Api Telegram webhook
	WebhookServer=telegram_bot_api_webhook_server:name_server(WebhookWhiteIP,WebhookPort1),
    
	_WebhookResult=telegram_bot_api_sup:start_webhook(#{
								id=>WebhookServer,
								secret_token=>WebhookSecretToken,
								bots=>#{
									%% add 1 bot
									BotName1Bin=>#{
												event=>{global,BotEvent1},
												name=>BotName1 	%%name atom
											}
										%%.. other bot
								},
								%%ranch_ssl:opts(),
								https=>#{
									ip=>{0,0,0,0},
									port=>WebhookPort,
									certfile=>Certfile,
									keyfile=>Keyfile,
									verify=> verify_none
								}
							}),
    %   {ok,_WebhookPid}=case WebhookResult of
    %         {error, {already_started, PidWh}} -> {ok, PidWh};
    %         PidWh -> PidWh
    %   end, 

	%%5. setWebhook (if not previously installed)
	WebhookUrl= telegram_bot_api_webhook_server:make_url(WebhookWhiteIP,WebhookPort1,BotName1Bin),
	io:format("WebhookUrl: ~p~n",[WebhookUrl]),
	try
	%%{ok,200,Result}
	Result=telegram_bot_api:setWebhook(BotName1,#{
		url=>WebhookUrl,
		ip_address=>WebhookWhiteIP,
		certificate=>#{
                 file=>Certfile,
                 name=><<"YOURPUBLIC.pem">>
                },
		allowed_updates=>[message,callback_query,channel_post,message_reaction,message_reaction_count],
		secret_token=>WebhookSecretToken
	}),
	io:format("Result setWebhook ~p~n",[Result]),
	ok
	catch
		E:M:S->
			io:format("Result setWebhook Error ~p + ~p + ~p~n",[E,M,S]),
			error
	end,
	io:format("global: ~p~n",[ets:tab2list(global_pid_names)]),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Add 2 bot dinamic
	% BotName2= bo2ghhgghg_bot,

	% BotEvent2 = binary_to_atom(list_to_binary(io_lib:format("webhook_event_BBB_~p", [BotName2]))),
	% {ok,_Pid22}=webhook_sup:start_child(BotEvent2),

	% Bot2= #{name=>BotName2,token=><<"8256699119:AAGgXDyF1i7WMTyosr1THQlx3G6y3awN2Ao">>},

	% ok=gen_event:add_handler({global,BotEvent2}, webhook_event_msg2, [Bot2]),
	% io:format("Handlers bot1:~p bot2:~p~n",[gen_event:which_handlers({global,BotEvent1}),gen_event:which_handlers({global,BotEvent2})]),
	
	% %  Create HTTP pool
	% {ok, _} = telegram_bot_api_sup:start_pool(Bot2#{
	% workers=>1
  	% }),

	% %%Add bot Webhook
	% BotName2Bin=atom_to_binary(BotName2),
	% ok= telegram_bot_api_webhook_server:add_bot(
	% 		{global,WebhookServer},%|| WebhookPid
	% 		BotName2Bin,
	% 		#{
	% 				event=>{global,BotEvent2},
	% 				name=>BotName2 	%%name atom
	% 		}
	% ),

	% io:format("global: ~p~n",[ets:tab2list(global_pid_names)]),

	% %%
	% %% setWebhook (if not previously installed)
	% WebhookUrl2= telegram_bot_api_webhook_server:make_url(WebhookWhiteIP,WebhookPort1,BotName2Bin),
 
	% io:format("WebhookUrl: ~p~n",[WebhookUrl2]),
	% try
	% {ok,200,Result2}=telegram_bot_api:setWebhook(BotName2,#{
	% 	url=>WebhookUrl2,
	% 	ip_address=>WebhookWhiteIP,
	% 	certificate=>#{
	% 			file=>Certfile,
	% 			name=><<"YOURPUBLIC.pem">>
	% 			},
	% 	allowed_updates=><<"[\"message\",\"callback_query\",\"channel_post\",\"message_reaction\",\"message_reaction_count\"]">>,
	% 	secret_token=>WebhookSecretToken
	% }),
	% io:format("Result2 setWebhook ~p~n",[Result2]),
	% ok
	% catch
	% 	E2:M2->
	% 		io:format("Result2 setWebhook Error ~p ~p~n",[E2,M2]),
	% 		error
	% end,

	init_systemd_notify(),
 
	{ok,Pid}.

stop(_State) ->
	systemd:notify(stopping),
    ok.


init_systemd_notify() ->
    Pid = os:getpid(),
    systemd:notify(ready),
    case os:getenv("WATCHDOG_PID") of
        false -> systemd:watchdog(enable);
        Pid -> systemd:watchdog(enable);
        _ -> false
    end.