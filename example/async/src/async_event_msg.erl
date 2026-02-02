-module(async_event_msg).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/message_effect.hrl").

init([Args]) ->
    {ok, Args}.

%sticker
handle_event({update,_BotName,#{
    message:=#{
            chat:=#{id:=ChatId},
            sticker:=Sticker
        }=_Message
    }=_M}, #{name:=Name}=State)->
    #{file_id:=FileId}=Sticker,
    Async=true,
    spawn(fun()->
        {ok,Ref} =telegram_bot_api:getFile(Name,#{file_id=>FileId},Async),
        io:format("getFile ref ~p~n",[Ref]),
        receive
            {async,Ref,Msg}->
                io:format("===============================~n",[]),
                io:format("getFile Msg ~p~n",[Msg]),
                case Msg of
                    {ok,200,#{result:=#{file_path:=FilePath}}}->
                        io:format("Sticker ~p~n",[FilePath]),
                        StreamTo=string:strip(os:cmd("mktemp"), right, $\n),
                        FileName=filename:basename(FilePath),
                        io:format("StreamTo ~p FileName ~p Pid ~p~n",[StreamTo,FileName,self()]),
                        {ok,Ref1} = telegram_bot_api_file:download(Name,FilePath,StreamTo,true),
                        io:format("Ref1 ~p ~p~n",[Ref1,self()]),
                        receive
                            {async,Ref1,saved_to_file}->
                                io:format("saved_to_file ~p~n",[StreamTo]),
                                Map1=#{
                                                        chat_id=>integer_to_binary(ChatId),
                                                        protect_content=>true,
                                                        sticker=>#{
                                                                file=>list_to_binary(StreamTo),
                                                                name=>FileName
                                                                }
                                                    },
                               {ok,RefSticker}=telegram_bot_api:sendSticker(Name,Map1,true),
                               io:format("RefSticker ~p~n",[RefSticker]),
                                file:delete(StreamTo),
                                ok
                            ;
                            _Err->
                                io:format("Msg1 _Err ~p~n",[_Err]),
                                error
                        end,
                        ok;
                    _->error
                    end,
                ok;
        _->error
        end
    end),
{ok, State};


handle_event({update,_BotName,#{
                    message:=#{
                            chat:=#{id:=ChatId},
                            text:=Text
                        }
                    }=_Message}, #{name:=Name}=State) ->
                        io:format("handle_event update ~p ~p~n",[_BotName,Text]),
    Async=true,                    
    Ref=telegram_bot_api:sendMessage(Name,#{
                            chat_id=>ChatId,
                            text=>Text,
                            message_effect_id=>?MESSAGE_EFFECT_FIRE
                            },Async),
    io:format("sendMessage ref:~p~n",[Ref]),
    {ok, State};
handle_event({error,_BotName,_Status,_Json}, State) ->
    io:format("handle_event error ~p ~p ~n",[_Status,_Json]),
    {ok, State};
handle_event(_Event, State) ->
    io:format("handle_event ~p~n",[_Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, no_reply, State}.

handle_info({async,Ref,Msg}, State) when is_reference(Ref)  ->
    io:format("handle_info ref ~p msg ~p~n",[Ref,Msg]),
    {ok, State};
handle_info(Info, State) ->
    io:format("Info ~p~n",[Info]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
