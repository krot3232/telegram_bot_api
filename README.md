# telegram_bot_api [![Hex Version](https://img.shields.io/hexpm/v/telegram_bot_api.svg)](https://hex.pm/packages/telegram_bot_api)

Telegram Bot API library

## Installation

The package can be installed by adding `telegram_bot_api` to your list of dependencies
in 
`rebar.config`:
```erlang
{deps, [telegram_bot_api]}.
```

## Basic Usage
``` erlang
    {ok, Pid} = telegram_bot_api_sup:start_pool(#{
        name=>mybot,
        token=><<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
        workers=>1
      }),                  
    Result=telegram_bot_api:sendMessage(mybot,#{
                            chat_id=>1234567,
                            text=><<"Text">>
                            }).
```
## Async send
``` erlang  
    Async=true,             
    {ok,Ref}=telegram_bot_api:sendMessage(mybot,#{
                            chat_id=><<"@channelusername">>,
                            text=><<"Text123">>
                            },Async).
```
## Long polling
``` erlang  
{ok, Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>mybot_pool1,
        token=><<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
        workers=>1
    }). 
    %% start_update use http pool created telegram_bot_api_sup:start_pool
 {ok, _Pid2} = telegram_bot_api_sup:start_update(#{
          name=>mybot_pool1,
          update_time=>1000,
          offset=>0,
          limit=>100,
          %%new msg are sent like this gen_event:notify(Event, {update, Name, Msg}), you can use the gen_event
          event=>spawn(fun F()-> 
                    %%messages    
                    receive
                        E-> io:format("event ~p~n",[E]),
                            F()
                    end
            end),
          allowed_updates=>[message]
    }).
```

## Webhook
``` erlang  
%%Start receive messages using webhook
{ok, Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>mybot_pool1,
        token=><<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
        workers=>1
    }). 
{ok,_WebhookPid}=telegram_bot_api_sup:start_webhook(#{
								secret_token=><<"my_secret">>,%%sent when installing a webhook telegram_bot_api:setWebhook..
								bots=>#{
									%% add 1 bot
									<<"mybot_pool1">>=>#{
												event=>{global,my_event},% the message will come here
												name=>mybot_pool1 
											}
										%%.. other bot
								},
								%%ranch_ssl:opts(),
								https=>#{
									ip=>{0,0,0,0},
									port=>8443,
									certfile=>"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem",
									keyfile=>"/etc/telegram_bot_api/ssl/YOURPRIVATE.key"
								}
							}).
```


## Other examples
See [/example](https://github.com/krot3232/telegram_bot_api/tree/main/example).
