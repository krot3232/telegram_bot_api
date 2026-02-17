# telegram_bot_api 
[![Hex Version](https://img.shields.io/hexpm/v/telegram_bot_api.svg?style=flat-square)](https://hex.pm/packages/telegram_bot_api)
[![Compatible with Bot API v9.4](https://img.shields.io/badge/Bot%20API%20version-v9.4-blue?style=flat-square)](https://core.telegram.org/bots/api#february-9-2026)

Erlang library for Telegram Bot API.  
It contains all methods and types available in Telegram Bot API 9.4, released February 9, 2026.

## Installation

The package can be installed by adding `telegram_bot_api` to your list of dependencies
in 
`rebar.config`:
```erlang
{deps, [telegram_bot_api]}.
```

## Basic usage
``` erlang
%%Use this token issued @BotFather
Token = <<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
%%After creating an HTTP pool, you can use any methods available in the Telegram API. 
Pool=mybot_pool,  
{ok, Pid} = telegram_bot_api_sup:start_pool(#{name=>Pool,token=>Token,workers=>1}),

%%Once the pool is created, you can call any methods as they are named in the Telegram documentation: telegram_bot_api:'Method'
%%The names of the methods from the Telegram documentation and the telegram_bot_api module are the same.
%%The first parameter is the pool, the second is the request parameters sent to Telegram, the third indicates asynchrony, and the fourth parameter is the call timeout.
%%See https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html. 
%%Example send message
ChatId=<<"@channelusername">>,%% or number
Async=false,%%*if you install True, the query result will be sent to the process mailbox
Timeout=5000,%%*default timeout for gen_server:call
Params=#{chat_id=>ChatId,text=><<"Text">>},
Result=telegram_bot_api:sendMessage(Pool,Params,Async,Timeout),
%%default Timeout=5000
%%Result=telegram_bot_api:sendMessage(Pool,Params,Async),
%%default Async=false
%%Result=telegram_bot_api:sendMessage(Pool,Params),
case Result of
    {ok, HttpCode, Map}->ok;
    {error, Er}->error;
    {Error, Reason}->error
end.


```
## Async send
``` erlang  
Async=true,             
{ok,Ref}=telegram_bot_api:sendMessage(Pool,#{chat_id=><<"@channelusername">>,text=><<"Text123">>},Async).
```
## Long polling
``` erlang  
{ok, Pid} = telegram_bot_api_sup:start_update(#{
    name=>Pool,
    update_time=>1000,
    offset=>0,
    limit=>100,
    %%new msg are sent like this gen_event:notify(Event, {update, Name, Msg}), you can use the gen_event
    event=>{global,my_event},% or pid
    allowed_updates=>[message]
}).
```
## Webhook
``` erlang  
{ok,WebhookPid}=telegram_bot_api_sup:start_webhook(#{
    secret_token=><<"my_secret">>,%this is the secret_token that is set in the Parameter method setWebhook https://core.telegram.org/bots/api#setwebhook https://core.telegram.org/bots/webhooks
    bots=>#{
    %% add 1 bot
    atom_to_binary(Pool)=>#{
    event=>{global,my_event},% the message will come here
    name=>Pool 
    }
    %%.. other bot
    },
    %%Parameter cowboy:start_tls TransportOpts :: ranch_ssl:opts(), see https://ninenines.eu/docs/en/ranch/2.2/manual/ranch_ssl/#_opt
    transport_opts=>#{
        ip=>{0,0,0,0},
        port=>8443,
        %% Optional parameter
        certfile=>"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem",
        keyfile=>"/etc/telegram_bot_api/ssl/YOURPRIVATE.key",
        verify=> verify_none,
        versions=> ['tlsv1.2'],
        fail_if_no_peer_cert=>false,
        log_level=>none,
        next_protocols_advertised=> [<<"http/1.1">>],
        alpn_preferred_protocols=> [<<"http/1.1">>]
    },
    %% Optional parameter
    protocol_opts=>#{
        alpn_default_protocol =>http
	}
}).
```
## Webhook dynamic add bot
``` erlang  
%%add
ok= telegram_bot_api_webhook_server:add_bot(
    {global,WebhookServer},%|| WebhookPid
    <<"mybot_pool">>,
    #{
        event=>{global,BotEvent1},
        name=>mybot_pool
    }
),
%%delete
    telegram_bot_api_webhook_server:delete_bot({global,WebhookServer},mybot_pool).
```
## Set webhook
``` erlang  
telegram_bot_api:setWebhook(BotName1,#{
		url=>telegram_bot_api_webhook_server:make_url(<<"8.8.8.8">>, <<"8443">>, <<"mybot_pool">>),%%make url: https://8.8.8.8:8443/telegram/mybot_pool/update
		ip_address=><<"8.8.8.8">>,
		certificate=>#{
                 file=><<"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem">>,
                 name=><<"YOURPUBLIC.pem">>
                },
		secret_token=><<"my_secret">>%%This token will be checked inside the handler cowboy, must match start_webhook
	}).
```
---
## Get me
``` erlang  
 {ok,200,Result} = telegram_bot_api:getMe(BotName,#{}).
```
## Log out
``` erlang  
{ok,200,#{ok := true,result := true}} = telegram_bot_api:logOut(BotName,#{}).
```
## Set bot name
``` erlang  
telegram_bot_api:setMyName(BotName,#{name=><<"Бот">>,language_code=>ru}).
```
## Set bot photo
``` erlang  
telegram_bot_api:setMyProfilePhoto(BotName,#{
                                    photo=>
                                            #{
                                            type=><<"static">>,
                                            photo=><<"attach://myfile">>
                                        },
                                        myfile=>#{
                                            file=><<"/etc/telegram_bot_api/file.jpg">>,
                                             name=><<"file.jpg">>
                                           }
                                        }).
```
## Set bot commands
``` erlang  
telegram_bot_api:setMyCommands(BotName,#{
    commands=>[
        #{
        command=><<"/test">>,
        description=><<"Test>>
        }]
    }).
```
## Send contact
``` erlang  
telegram_bot_api:sendContact(BotName,#{
         chat_id=>ChatId,
         phone_number=><<"+79281111111">>,
         first_name=><<"CONTACT">>
        })
```
## Send photo
``` erlang  
Parameter=#{
    chat_id=><<"@channelusername">>,
    photo=>#{file=><<"/dir/file.jpg">>,name=><<"file.jpg">>}
    },
telegram_bot_api:sendPhoto(Pool,Parameter).
```
## Set message reaction
``` erlang 
-include_lib("telegram_bot_api/include/message_reaction.hrl"). 
telegram_bot_api:setMessageReaction(BotName,#{
        chat_id=>ChatId,
        message_id=>MessageId,
        reaction=>[
        #{
            type=>?REACTION_TYPE_EMOJI,%% or telegram_bot_api_emoji:random_reaction()
            emoji=>Reaction1
        }
    ]},Async),
```
## Send message effect
``` erlang 
-include_lib("telegram_bot_api/include/message_effect.hrl").
   telegram_bot_api:sendMessage(BotName,#{
        chat_id=>ChatId,
        text=><<"text">>,
        message_effect_id=>?MESSAGE_EFFECT_FIRE
    }).
```
## Edit message text
``` erlang 
telegram_bot_api:editMessageText(BotName,#{
        chat_id=>ChatId,
        text=><<"text">>,
        message_id=>MessageId
}).
```  
## Inline keyboard
``` erlang 
telegram_bot_api:sendMessage(BotName,#{
         chat_id=>ChatId,
         text=><<"inline_keyboard">>,
         reply_markup=>
                 #{
                 inline_keyboard=>
                 [
                         [
                         #{
                             text=><<"yes">>,
                             callback_data=><<"callback_yes">>
                         },
                         #{
                             text=><<"no">>,
                             callback_data=><<"callback_no">>
                         }
                         ]
                 ]
             }
    }).
```
## Edit message reply markup
``` erlang 
telegram_bot_api:editMessageReplyMarkup(BotName,#{
                message_id=>MessageId,
                chat_id=>ChatId,
                reply_markup=>
                    #{
                    inline_keyboard=>
                    [
                            [
                            #{
                                text=><<"ok">>,
                                callback_data=><<"callback_ok">>
                            }
                            ]
                    ]
                }
                }).
```
## Answer callback query
``` erlang 
telegram_bot_api:answerCallbackQuery(BotName,#{
    callback_query_id=>Id
    }).
```
## Send chat action
``` erlang 
-include_lib("telegram_bot_api/include/chat_action.hrl").
    telegram_bot_api:sendChatAction(BotName,#{
        chat_id=>ChatId,
        action=>?CHAT_ACTION_UPLOAD_PHOTO
    }).
```
## Send dice
``` erlang 
-include_lib("telegram_bot_api/include/message_dice.hrl").
    Result=telegram_bot_api:sendDice(BotName,#{
             chat_id=>ChatId,
             emoji=>?DICE_BOWLING,%% or telegram_bot_api_emoji:random_dice()
             protect_content=>true
     }).
     %%
     %%{ok,200,#{ok := true,result :=#{ message_id := MessageId,  chat:=#{id:=ChatId}, dice :=#{value :=Value emoji:= Emoji}  } }}=Result,
     %%IsWin=telegram_bot_api_emoji:is_win_dice(Emoji,Value). %% true or false
```
## Send message draft
``` erlang 
telegram_bot_api:sendMessageDraft(BotName,#{
         chat_id=>ChatId, 
         draft_id=>DrafId,
         text=>Text,
         message_thread_id=>ThreadId
     })
```
## Create forum topic
``` erlang 
-include_lib("telegram_bot_api/include/message_topic.hrl").
   telegram_bot_api:createForumTopic(BotName,#{
        chat_id=>ChatId,
        name=><<"name topic">>,
        icon_color=>?TOPIC_ICON_COLOR_CREAMY
        icon_custom_emoji_id=>?TOPIC_EMOJI_NEWSPAPER_ID %% or telegram_bot_api_emoji:random_topic()
    }).
```
## Edit forum topic
``` erlang 
telegram_bot_api:editForumTopic(BotName,#{
    chat_id=>ChatId,
    message_thread_id=>ThreadId,
    name=>Text
    }).
```
## Get user profile audios
``` erlang 
telegram_bot_api:getUserProfileAudios(pool_mybot1,#{user_id=>1234}).
```

## Other
* [all methods](https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html#bot-settings)
* [emoji](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html)
* [example](https://github.com/krot3232/telegram_bot_api/tree/main/example)   



