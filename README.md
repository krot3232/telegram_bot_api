# telegram_bot_api 
<img src="https://raw.githubusercontent.com/krot3232/logos/main/telegram_bot_api.png" width="200">

Erlang library for Telegram Bot API.  
It contains all methods and types available in Telegram Bot API 9.6, released April 3, 2026.

[![Erlang](https://img.shields.io/badge/Erlang%2FOTP-27+-deeppink?style=flat-square&logo=erlang&logoColor=ffffff)](https://www.erlang.org)
[![Hex Version](https://img.shields.io/hexpm/v/telegram_bot_api.svg?style=flat-square)](https://hex.pm/packages/telegram_bot_api)
[![Hex Docs](https://img.shields.io/badge/hex-docs-orange?style=flat-square)](https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html)
[![Compatible with Bot API v9.6](https://img.shields.io/badge/Bot%20API%20version-v9.6-success?style=flat-square)](https://core.telegram.org/bots/api#april-3-2026)

## Installation

The package can be installed by adding `telegram_bot_api` to your list of dependencies
in 
`rebar.config`:
```erlang
{deps, [telegram_bot_api]}.
```

## Basic usage
``` erlang
%1. Get a Telegram API token, use telegram bot @BotFather for receiving a token
Token = <<"8803758952:Qo2xCv3DAGxyJyXePA8S4dYN-BzSkUEbqFj">>,

%2. Create pool
%%Each bot is a separate pool of workers
%%Bot -> Pool -> Workers -> Telegram API
Pool=mybot1_pool,
{ok, Pid} = telegram_bot_api_sup:start_pool(#{
	name=>Pool,
	token=>Token,
	workers=>1
	}), 
%See param https://hexdocs.pm/telegram_bot_api/telegram_bot_api_sup.html#start_pool/1
%%ok=telegram_bot_api_sup:stop_pool(Pool).

%3. Send request Telegram Bot Api
%%After creating an HTTP pool, you can use any methods available in the Telegram API. 
%%Once the pool is created, you can call any methods as they are named in the Telegram documentation: telegram_bot_api:'Method'
%%The names of the methods from the Telegram documentation and the telegram_bot_api module are the same.
%%The first parameter is the pool, the second is the request parameters sent to Telegram, the third indicates asynchrony, and the fourth parameter is the call timeout.

ChatId=<<"@channelusername">>,
Params=#{chat_id=>ChatId,...},%%parameters are always maps
Async=false,%*if you install Async=true, the query result will be sent to the process mailbox message: {async,Ref,{ok,HttpCode,MapJson}} or {async, Ref, saved_to_file}(use in telegram_bot_api_file:download) or {error, Reason}
Timeout=5000,%*default timeout for gen_server:call
Method = ..., % sendMessage or sendMessageDraft or sendVoice or any method
Result=telegram_bot_api:Method(Pool,Params,Async,Timeout),
case Result of
    {ok, Ref}->ok; %is Async=true
    {ok, HttpCode, MapJson}->ok; %is Async=false
    {error, Er}->error;
    {Error, Reason}->error
end.
```
[all methods](https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html#message)
## Send message
sendMessage → pool → worker → Telegram
``` erlang            
{ok,200,Result} = telegram_bot_api:sendMessage(Pool,#{
        chat_id=>ChatId,
        text=><<"Text">>
        }
        ).
```
## Async send message
``` erlang             
{ok,Ref} = telegram_bot_api:sendMessage(Pool,#{
        chat_id=>ChatId,
        text=><<"Text">>
        },
        true).
%%the response will come as process messages
receive
   {async,Ref,{ok,200,Map}}->ok
end.
```
[`telegram_bot_api:sendMessage`](https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html#sendMessage/4)

---
## Long polling
``` erlang  
{ok, Pid1}=gen_event:start_link({global, my_event}),
gen_event:add_handler(my_event, my_event_handler1, [#{name=>Pool}]),

{ok, Pid2} = telegram_bot_api_sup:start_update(#{
    name=>Pool,
    update_time=>1000,
    offset=>0,
    limit=>100,
    event=>{global,my_event},% or Pid1. new messages will be sent to this process
    allowed_updates=>[message]% see https://hexdocs.pm/telegram_bot_api/telegram_bot_api.html#t:update_type/0
}).
```
[`telegram_bot_api_sup:start_update`](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_sup.html#start_update/1)
[`telegram_bot_api_updater_server`](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_updater_server.html)  


## Webhook
``` erlang  
WebhookId=telegram_bot_api_webhook_server:name_server({0,0,0,0},8443),
{ok,WebhookPid}=telegram_bot_api_sup:start_webhook(#{
    id=>WebhookId,%% name process, may not be specified, then create an ID for the supervisor by calling telegram_bot_api_webhook_server:name_server
    secret_token=><<"my_secret">>,%this is the secret_token that is set in the Parameter method setWebhook https://core.telegram.org/bots/api#setwebhook
    bots=>#{
	%%set bots when creating a webhook or add them later via add_bot
	%% add 1 bot
	atom_to_binary(Pool)=>#{
	event=>{global,my_event},% the message will come here
	name=>Pool 
    }
     %%.. other bot
    },
    transport_opts=>#{
        ip=>{0,0,0,0},
        port=>8443,
        %% Optional parameter
        certfile=>"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem",
        keyfile=>"/etc/telegram_bot_api/ssl/YOURPRIVATE.key",
        verify=> verify_none
    }
}).
{ok,WebhookPid}=global:whereis_name(WebhookId).
```

[`telegram_bot_api_sup:start_webhook`](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_sup.html#start_webhook/1) 
[`telegram_bot_api_webhook_server`](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_webhook_server.html)  

## Webhook dynamic add bot
``` erlang  
%%add
ok= telegram_bot_api_webhook_server:add_bot(
    {global,WebhookId},%or WebhookPid
    <<"mybot_pool">>,
    #{
        event=>{global,my_event},
        name=>mybot_pool
    }
),
%%delete
    telegram_bot_api_webhook_server:delete_bot({global,WebhookId},mybot_pool).
```
## Set webhook
``` erlang  
telegram_bot_api:setWebhook(Pool1,#{
		url=>telegram_bot_api_webhook_server:make_url(<<"8.8.8.8">>, <<"8443">>, <<"mybot_pool">>),% make url: https://8.8.8.8:8443/telegram/mybot_pool/update
		ip_address=><<"8.8.8.8">>,
		certificate=>#{
                 file=><<"/etc/telegram_bot_api/ssl/YOURPUBLIC.pem">>,
                 name=><<"YOURPUBLIC.pem">>
                },
		secret_token=><<"my_secret">>% this token will be checked inside the handler cowboy, must match start_webhook
	}).
```
[telegram_bot_webhook_example](https://github.com/krot3232/telegram_bot_webhook_example/)

---
## Get me
``` erlang  
 {ok,200,Result} = telegram_bot_api:getMe(Pool,#{}).
```
## Log out
``` erlang  
{ok,200,#{ok := true,result := true}} = telegram_bot_api:logOut(Pool,#{}).
```
## Set bot name
``` erlang  
telegram_bot_api:setMyName(Pool,#{name=><<"Бот">>,language_code=>ru}).
```
## Set bot photo
``` erlang  
telegram_bot_api:setMyProfilePhoto(Pool,#{
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
telegram_bot_api:setMyCommands(Pool,#{
    commands=>[
        #{
        command=><<"/test">>,
        description=><<"Test>>
        }]
    }).
```
## Send contact
``` erlang  
telegram_bot_api:sendContact(Pool,#{
         chat_id=>ChatId,
         phone_number=><<"+79281111111">>,
         first_name=><<"CONTACT">>
        })
```
## Send photo
``` erlang  
telegram_bot_api:sendPhoto(Pool,#{
        chat_id=><<"@channelusername">>,
        photo=>#{file=><<"/dir/file.jpg">>,name=><<"file.jpg">>}
        }).
```
## Send audio
``` erlang  
telegram_bot_api:sendAudio(Pool,#{
        chat_id=>ChatId,
        audio =>#{file=><<"/dir/file.mp3">>,name=><<"file.mp3">>}
        }).
```
## Set message reaction
``` erlang 
-include_lib("telegram_bot_api/include/message_reaction.hrl"). 
telegram_bot_api:setMessageReaction(Pool,#{
        chat_id=>ChatId,
        message_id=>MessageId,
        reaction=>[
        #{
            type=>?REACTION_TYPE_EMOJI, % REACTION_TYPE_EMOJI | REACTION_TYPE_CUSTOM_EMOJI | REACTION_TYPE_PAID see https://core.telegram.org/bots/api#reactiontype
            emoji=>?REACTION_OK_HAND % or telegram_bot_api_emoji:random_reaction()
        }
    ]},Async),
```
[macros message reaction](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html#module-reaction)
## Send message effect
``` erlang 
-include_lib("telegram_bot_api/include/message_effect.hrl").
   telegram_bot_api:sendMessage(Pool,#{
        chat_id=>ChatId,
        text=><<"text">>,
        message_effect_id=>?MESSAGE_EFFECT_FIRE
    }).
```
[macros message effect](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html#module-effect)

## Edit message text
``` erlang 
telegram_bot_api:editMessageText(Pool,#{
        chat_id=>ChatId,
        text=><<"text">>,
        message_id=>MessageId
}).
```  
## Edit message caption
``` erlang 
telegram_bot_api:editMessageText(Pool,#{
        chat_id=>ChatId,
        message_id=>MessageId,
        parse_mode =><<"HTML">>,
        caption=><<"<a href=\"tg://user?id=123\">User</a><code>123</code>">>
}).
```  
## Delete message
``` erlang 
telegram_bot_api:deleteMessage(Pool,#{
        chat_id=>ChatId,
        message_id=>MessageId
}).
```  
## Inline keyboard
``` erlang 
telegram_bot_api:sendMessage(Pool,#{
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
## Inline keyboard style button
``` erlang 
telegram_bot_api:sendMessage(Pool,#{
        chat_id=>ChatId,
        text=><<"inline_keyboard_style">>,
        reply_markup=>
                #{
                inline_keyboard=>
                [
                [
                #{
                    text=><<"red">>,
                    style=><<"danger">>,
                    callback_data=><<"callback_red">>
                },
                #{
                    text=><<"green">>,
                    style=><<"success">>,
                    callback_data=><<"callback_green">>
                },
                #{
                    text=><<"blue">>,
                    style=><<"primary">>,
                    callback_data=><<"callback_blue">>
                },
                #{
                text=><<"default">>,
                callback_data=><<"callback_default">>
                }
                ]
                ]
            }
        }).
```
## Answer callback query
``` erlang 
telegram_bot_api:answerCallbackQuery(Pool,#{
    callback_query_id=>Id
    }).
```
## Edit message reply markup
``` erlang 
telegram_bot_api:editMessageReplyMarkup(Pool,#{
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
## Send chat action
``` erlang 
-include_lib("telegram_bot_api/include/chat_action.hrl").
    telegram_bot_api:sendChatAction(Pool,#{
        chat_id=>ChatId,
        action=>?CHAT_ACTION_UPLOAD_PHOTO
    }).
```
## Send dice
``` erlang 
-include_lib("telegram_bot_api/include/message_dice.hrl").
    Result=telegram_bot_api:sendDice(Pool,#{
             chat_id=>ChatId,
             emoji=>?DICE_BOWLING,% or telegram_bot_api_emoji:random_dice()
             protect_content=>true
     }).
     %%
     %%{ok,200,#{ok := true,result :=#{ message_id := MessageId,  chat:=#{id:=ChatId}, dice :=#{value :=Value emoji:= Emoji}  } }}=Result,
     %%IsWin=telegram_bot_api_emoji:is_win_dice(Emoji,Value). % true or false
```
[macros message dice](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html#module-dice)
## Send message draft
``` erlang 
telegram_bot_api:sendMessageDraft(Pool,#{
         chat_id=>ChatId, 
         draft_id=>DrafId,
         text=>Text,
         message_thread_id=>ThreadId
     })
```
## Create forum topic
``` erlang 
-include_lib("telegram_bot_api/include/message_topic.hrl").
   telegram_bot_api:createForumTopic(Pool,#{
        chat_id=>ChatId,
        name=><<"name topic">>,
        icon_color=>?TOPIC_ICON_COLOR_CREAMY
        icon_custom_emoji_id=>?TOPIC_EMOJI_NEWSPAPER_ID % or telegram_bot_api_emoji:random_topic()
    }).
```
[macros message topic](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html#module-forum-topic)
## Edit forum topic
``` erlang 
telegram_bot_api:editForumTopic(Pool,#{
    chat_id=>ChatId,
    message_thread_id=>ThreadId,
    name=>Text
    }).
```
## Mute chat member
``` erlang 
mute_chat_member(Pool, ChatId, UserId, Minute) ->
    Result = telegram_bot_api:restrictChatMember(Pool, #{
        chat_id => ChatId,
        user_id => UserId,
        until_date => erlang:system_time(seconds) + (60 * Minute),
        permissions => #{
            can_send_messages => false,
            can_send_audios => false,
            can_send_documents => false,
            can_send_photos => false,
            can_send_videos => false,
            can_send_video_notes => false,
            can_send_voice_notes => false,
            can_send_polls => false,
            can_send_other_messages => false,
            can_add_web_page_previews => false,
            can_change_info => false,
            can_invite_users => false,
            can_pin_messages => false,
            can_manage_topics => false
        }
    }),
    case Result of
        {ok, 200, #{ok := true, result := true}} -> true;
        _ -> false
    end.
```
## Bun chat member
``` erlang 
ban_chat_member(BotName, ChatId, UserId, Minute) ->
    Result = telegram_bot_api:banChatMember(BotName, #{
        chat_id => ChatId,
        user_id => UserId,
        until_date => erlang:system_time(seconds) + (60 * Minute)
    }),
    case Result of
        {ok, 200, #{ok := true, result := true}} -> true;
        _ -> false
    end.
```
## Get chat member
``` erlang 
%%Get a user role in a group
case telegram_bot_api:getChatMember(Pool, #{
        chat_id=>ChatId,
        user_id=>UserId
        }) of
        {ok,200, #{ok := true, result := #{status := Status} } }->
          Status;%creator or administrator or member or restricted or left or kicked; see type ChatMember 
        _->false
        end.
        
```
## Get user profile audios
``` erlang 
telegram_bot_api:getUserProfileAudios(Pool,#{user_id=>1234}).
```
## Set chat member tag
``` erlang 
case telegram_bot_api:setChatMemberTag(Pool, #{
        chat_id=>ChatId,
        user_id=>UserId,
        tag=><<"tag">>
        }) of
        {ok,200,#{ok := true,result := true}}->ok;
        %If you can't install the tag, you'll get an error example {ok,400,#{ok => false,description => <<"Bad Request: CHAT_CREATOR_REQUIRED">>,error_code => 400}} ->error;
        _->error
        end.
```

## See

* [Examples](https://github.com/krot3232/telegram_bot_api/tree/main/example) 

## Macros 
* [Message Emojis](https://hexdocs.pm/telegram_bot_api/telegram_bot_api_emoji.html#module-emoji)
