-module(webhook_event_msg).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("telegram_bot_api/include/message_reaction.hrl").
-include_lib("telegram_bot_api/include/message_dice.hrl").
-include_lib("telegram_bot_api/include/message_effect.hrl").
-include_lib("telegram_bot_api/include/message_topic.hrl").
-include_lib("telegram_bot_api/include/chat_action.hrl").
-include_lib("telegram_bot_api/include/emoji.hrl").
init([Args]) ->
    io:format("init ~p ~p~n",[self(),Args]),
    {ok, Args}.


handle_event({update,BotName,#{callback_query:=CallbackQuery} =_Message}, State) ->
    #{id:=Id,data:=Data,message :=#{chat:=#{id:=ChatId},message_id:=MessageId}}=CallbackQuery,
    io:format("handle_event update callback_query [BOT1] ~p ~p~n",[Id,Data]),
    
    Result=case Data of
        <<"callback_yes_y">>->
              telegram_bot_api:editMessageReplyMarkup(BotName,#{
                message_id=>MessageId,
                chat_id=>ChatId,
                reply_markup=>
                    #{
                    inline_keyboard=>
                    [
                            [
                            #{
                                text=>?EMOJI_X,
                                callback_data=><<"callback_yes">>
                            }
                            ]
                    ]
                }
                }),
                telegram_bot_api:answerCallbackQuery(BotName,#{
                    callback_query_id=>Id
                    });
        <<"callback_yes">>->
           _X= telegram_bot_api:editMessageReplyMarkup(BotName,#{
            message_id=>MessageId,
            chat_id=>ChatId,
            reply_markup=>
                #{
                inline_keyboard=>
                [
                        [
                        #{
                            text=>?EMOJI_WHITE_CHECK_MARK,
                            callback_data=><<"callback_yes_y">>
                        }
                        ]
                ]
            }
            }),
           % io:format("editMessageReplyMarkup: ~p~n",[X]),
            telegram_bot_api:answerCallbackQuery(BotName,#{
                callback_query_id=>Id,
                text=>  ?EMOJI_LION_FACE
                %show_alert=>true,
                %url=> <<"https://t.me/test_bot?start=Cmd123">>
                });
        <<"callback_no">>->   
            _X= telegram_bot_api:editMessageReplyMarkup(BotName,#{
                message_id=>MessageId,
                chat_id=>ChatId,
                reply_markup=>
                    #{
                    inline_keyboard=>
                    [
                            [
                            #{
                                text=>?EMOJI_MUSICAL_SCORE,
                                callback_data=><<"callback_none">> %%1-64 bytes
                            }
                            ]
                    ]
                }
                }),
                telegram_bot_api:answerCallbackQuery(BotName,#{callback_query_id=>Id,text=>?EMOJI_MUSICAL_NOTE});
         _->
            telegram_bot_api:answerCallbackQuery(BotName,#{callback_query_id=>Id})
        end,

    io:format("answerCallbackQuery: ~p~n",[Result]),


     
    {ok,State};
handle_event({update,BotName,#{
                            message:=#{
                                    chat:=#{id:=ChatId},
                                    text:=Text,
                                    message_id:=MessageId
                                }
                            }=_Message
}, State) ->
    io:format("\e[0;41mhandle_event update [BOT1] ~p |~ts|\e[0m~n",[BotName,Text]),
     Async=true,
    %%send reaction
    % Reaction = [?REACTION_THUMBSUP,?REACTION_OK_HAND,?REACTION_SALUTING_FACE,?REACTION_EYES,?REACTION_WRITING_HAND],
    % Reaction = ?REACTIONS,
    %Reaction = [?REACTION_SALUTING_FACE],
    Reaction1 = telegram_bot_api_emoji:random_reaction(),
     {ok,_Ref}=telegram_bot_api:setMessageReaction(BotName,#{
             chat_id=>ChatId,
             message_id=>MessageId,
             reaction=>[
                 #{
                 type=>?REACTION_TYPE_EMOJI,
                 emoji=>Reaction1
                 }
             ]},Async),

    _Result=telegram_bot_api:sendMessage(BotName,#{
        chat_id=>ChatId,
        text=><<"inline_keyboard">>,
        message_effect_id=>?MESSAGE_EFFECT_FIRE,
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
        }),  
        %io:format("Result ~p~n",[Result]),


            %%change my name
            %  {async,#Ref<0.1775988704.458227713.31718>,
            %  {ok,429,
            %      #{ok => false,
            %        description =>
            %            <<"Too Many Requests: retry after 86362">>,
            %        error_code => 429,
            %        parameters => #{retry_after => 86362}}}}       
            %  Result=telegram_bot_api:setMyName(BotName,#{name=>Reaction1,language_code=>ru}),
            %  case Result of
            %     {ok,429, #{ok := false,
            %     description := Msg,%<<"Too Many Requests: retry after 86267">>,
            %     error_code := 429,
            %     parameters := #{retry_after := After}}  } -> 
            %     io:format("setMyName error ~p ~p~n",[Msg,After]), 
            %     error;
            % {ok,200,_Msg}->
            %     ok;
            % _Err->error
            % end,
            %  io:format("setMyName ~p~n",[Result]), 

    % send chat action         
    % telegram_bot_api:sendChatAction(BotName,#{
    %         chat_id=>ChatId,
    %         action=>?CHAT_ACTION_TYPING
    % }),

    %% send rand 1-6
%    {ok,_Ref1}=telegram_bot_api:sendDice(BotName,#{
%             chat_id=>ChatId,
%             emoji=>?DICE_BOWLING,
%             message_effect_id=>?MESSAGE_EFFECT_FIRE,
%             protect_content=>true,
%             reply_markup=>#{
%                      force_reply=>true,
%                      input_field_placeholder=><<"Message....">>,
%                      %%1 user
%                      selective=>true                
%                     }
%     },Async),
   %% send echo text 
   % {ok,200,_Result1}=telegram_bot_api:sendMessage(BotName,#{chat_id=>ChatId,text=>Text}),

%% add group
%    Rights=#{
%     %is_anonymous=>false,
%     can_manage_chat=>true,
%     %can_delete_messages=>true,
%     %can_manage_video_chats=>false,
%     %can_restrict_members=>true,
%     %can_promote_members=>false,

%     %can_change_info=>false,
%     %can_invite_users=>false,

%     %can_post_stories=>false,
%     %can_edit_stories=>false,
%     %can_delete_stories=>false,

%     can_post_messages=>true
%     %can_edit_messages=>true
%     %can_pin_messages=>true
%     %can_manage_topics=>true
%     %can_manage_direct_messages=>true
%     },
%     {ok,200,_Result2}=telegram_bot_api:sendMessage(BotName,#{
%         chat_id=>ChatId,
%         text=><<">>>">>,
%         reply_markup=>
%                                 #{
%                                     keyboard=>[[
%                                     #{
%                                         text=><<"send">>,
%                                         request_chat=>#{
%                                                 request_id=> erlang:unique_integer([positive]),
%                                                 %%channel
%                                                 %chat_is_channel=>true,
%                                                 %chat
%                                                 bot_is_member=>true,

%                                                  %chat_has_username=>true,
%                                                  %chat_is_created=>true,

%                                                 % request_title=>true,
%                                                 % request_username=>true,
                                            
%                                                 %request_photo=>true
%                                                 bot_administrator_rights=>Rights,
%                                                 user_administrator_rights=>Rights
%                                             }
%                                        % request_contact=>true,
%                                        % request_location=>true
%                                     }
%                                 ]]  
%                                 }
                            
%     }),

 %% send create topic
    % {ok,200,_Result1}=telegram_bot_api:createForumTopic(BotName,#{
    %                 chat_id=>ChatId,
    %                 name=>Text,
    %                 %icon_color=>?TOPIC_ICON_COLOR_CREAMY
    %                 icon_custom_emoji_id=>lists:nth(rand:uniform(length(?TOPIC_EMOJI_IDS)), ?TOPIC_EMOJI_IDS)
    %             }),

 
    {ok, State};
  



handle_event({error,BotName,Err,Msg}, State) ->
    io:format("handle_event error ~p ~p ~p ~n",[BotName,Err,Msg]),
    {ok, State};
handle_event(_Event, State) ->
    io:format("handle_event ~p ~p~n",[_Event,State]),
    {ok, State}.

handle_call(_Request, State) ->
    io:format("handle_call ~p~n",[_Request]),
    {ok, no_reply, State}.

 
handle_info({async,Ref,{ok,200,#{ok := true,result := true}}}, State) ->
    io:format("Send ok ~p~n",[Ref]),
    {ok, State};
handle_info(_Info, State) ->
    io:format("handle_info ~p~n",[_Info]),
    {ok, State}.

terminate(_Args, _State) ->
    io:format("terminate ~p~n",[_Args]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("code_change ~p~n",[_OldVsn]),
    {ok, State}.
