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


handle_event({update,BotName,#{
                            message:= Msg=#{
                            chat:=#{id:=ChatId},
                            from:=#{id:=UserId},
                            text:=Text,
                            message_id:=MessageId
                            } 
                            }=_Result
}, State) ->
    io:format("\e[0;41mhandle_event update [BOT1] ~p <~p>:~p |~p| ~ts\e[0m~n",[BotName,ChatId,MessageId,UserId,Text]),

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
