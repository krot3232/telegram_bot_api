-module(echo_event_msg).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).



init([Args]) ->
    {ok, Args}.

%text
handle_event({update,_BotName,#{
                    message:=#{
                            chat:=#{id:=ChatId},
                            text:=Text
                        }
                    }=_Message}, #{name:=Name}=State) ->
    telegram_bot_api:sendMessage(Name,#{chat_id=>ChatId,text=>Text}),
    {ok, State};
%%photo
handle_event({update,_BotName,#{
        message:=#{
                chat:=#{id:=ChatId},
                photo:=Photos
            }=Message
        }=_M}, #{name:=Name}=State)->
        #{file_id:=FileId}=hd(lists:reverse(Photos)),
       Caption = maps:get(caption,Message,<<>>),
       io:format("photo ~p~n",[FileId]),
        case telegram_bot_api:getFile(Name,#{file_id=>FileId}) of
            {ok,200,#{result:=#{file_path:=FilePath}}} ->
                StreamTo=string:strip(os:cmd("mktemp"), right, $\n),
                case telegram_bot_api_file:download(Name,FilePath,StreamTo) of
                    saved_to_file ->
                    FileName=filename:basename(FilePath),
                    Map=#{
                                                            chat_id=>integer_to_binary(ChatId),
                                                            photo=>#{
                                                                    file=>list_to_binary(StreamTo),
                                                                    name=>FileName
                                                                    }
                                                        },
                    MapNew=if Caption=:=<<>>->  Map;                              
                    true->Map#{caption=>Caption}
                    end,
                    Result=telegram_bot_api:sendPhoto(Name,MapNew),
                    file:delete(StreamTo),
                    io:format("File download ok ChatId ~p StreamTo ~p ~p~n",[ChatId,StreamTo,Result])
                    ;
                  Err->  
                    io:format("Message Err ~p~n",[Err])
                    end,
                ok;
            Err->
                io:format("Message Err ~p~n",[Err]),
                error
        end,
    {ok, State};
%document
    handle_event({update,_BotName,#{
        message:=#{
                chat:=#{id:=ChatId},
                document:=Document
            }=Message
        }=_M}, #{name:=Name}=State)->
        #{file_id:=FileId,file_name:=FileName}=Document,
       Caption = maps:get(caption,Message,<<>>),
        case telegram_bot_api:getFile(Name,#{file_id=>FileId}) of
            {ok,200,#{result:=#{file_path:=FilePath}}} ->
                io:format("document ~p~n",[FilePath]),
                StreamTo=string:strip(os:cmd("mktemp"), right, $\n),

                case telegram_bot_api_file:download(Name,FilePath,StreamTo) of
                    saved_to_file ->
                    Map=#{
                                                            chat_id=>integer_to_binary(ChatId),
                                                            document=>#{
                                                                    file=>list_to_binary(StreamTo),
                                                                    name=>FileName
                                                                    }
                                                        },
                    MapNew=if Caption=:=<<>>->  Map;                              
                    true->Map#{caption=>Caption}
                    end,
                    Result=telegram_bot_api:sendDocument(Name,MapNew),
                    file:delete(StreamTo),
                    io:format("File download ok ChatId ~p StreamTo ~p ~p~n",[ChatId,StreamTo,Result])
                    ;
                  Err->  
                    io:format("Message Err ~p~n",[Err])
                    end,

                ok;
            Err->
                io:format("Message Err ~p~n",[Err]),
                error
        end,
    {ok, State}; 
%voice
    handle_event({update,_BotName,#{
        message:=#{
                chat:=#{id:=ChatId},
                voice:=Voice
            }=Message
        }=_M}, #{name:=Name}=State)->
        #{file_id:=FileId}=Voice,
       Caption = maps:get(caption,Message,<<>>),
        case telegram_bot_api:getFile(Name,#{file_id=>FileId}) of
            {ok,200,#{result:=#{file_path:=FilePath}}} ->
                io:format("voice ~p~n",[FilePath]),
                StreamTo=string:strip(os:cmd("mktemp"), right, $\n),
                FileName=filename:basename(FilePath),
                case telegram_bot_api_file:download(Name,FilePath,StreamTo) of
                    saved_to_file ->
                    Map=#{
                                                            chat_id=>integer_to_binary(ChatId),
                                                            voice=>#{
                                                                    file=>list_to_binary(StreamTo),
                                                                    name=>FileName
                                                                    }
                                                        },
                    MapNew=if Caption=:=<<>>->  Map;                              
                    true->Map#{caption=>Caption}
                    end,
                    Result=telegram_bot_api:sendVoice(Name,MapNew),
                    file:delete(StreamTo),
                    io:format("File download ok ChatId ~p StreamTo ~p ~p~n",[ChatId,StreamTo,Result])
                    ;
                  Err->  
                    io:format("Message Err ~p~n",[Err])
                    end,
                    
                ok;
            Err->
                io:format("Message Err ~p~n",[Err]),
                error
        end,
    {ok, State};

%video_note
handle_event({update,_BotName,#{
    message:=#{
            chat:=#{id:=ChatId},
            video_note:=VideoNote
        }=_Message
    }=_M}, #{name:=Name}=State)->
    #{file_id:=FileId}=VideoNote,
    case telegram_bot_api:getFile(Name,#{file_id=>FileId}) of
        {ok,200,#{result:=#{file_path:=FilePath}}} ->
            io:format("video_note ~p~n",[FilePath]),
            StreamTo=string:strip(os:cmd("mktemp"), right, $\n),
            FileName=filename:basename(FilePath),
            case telegram_bot_api_file:download(Name,FilePath,StreamTo) of
                saved_to_file ->
                Map=#{
                                                        chat_id=>integer_to_binary(ChatId),
                                                        video_note=>#{
                                                                file=>list_to_binary(StreamTo),
                                                                name=>FileName
                                                                }
                                                    },
                Result=telegram_bot_api:sendVideoNote(Name,Map),
                file:delete(StreamTo),
                io:format("File download ok ChatId ~p StreamTo ~p ~p~n",[ChatId,StreamTo,Result])
                ;
              Err->  
                io:format("Message Err ~p~n",[Err])
                end,
                
            ok;
        Err->
            io:format("Message Err ~p~n",[Err]),
            error
    end,
{ok, State};

%sticker
handle_event({update,_BotName,#{
    message:=#{
            chat:=#{id:=ChatId},
            sticker:=Sticker
        }=_Message
    }=_M}, #{name:=Name}=State)->
    #{file_id:=FileId}=Sticker,
    try
    case telegram_bot_api:getFile(Name,#{file_id=>FileId}) of
        {ok,200,#{result:=#{file_path:=FilePath}}} ->
            io:format("Sticker ~p~n",[FilePath]),
            StreamTo=string:strip(os:cmd("mktemp"), right, $\n),
            FileName=filename:basename(FilePath),
            case telegram_bot_api_file:download(Name,FilePath,StreamTo) of
              saved_to_file ->
                io:format("Sticker ~p~n",[ok]),
                Map=#{
                                                        chat_id=>integer_to_binary(ChatId),
                                                        protect_content=>true,
                                                        sticker=>#{
                                                                file=>list_to_binary(StreamTo),
                                                                name=>FileName
                                                                }
                                                    },
                Result=telegram_bot_api:sendSticker(Name,Map),
                file:delete(StreamTo),
                io:format("File download ok ChatId ~p StreamTo ~p ~p~n",[ChatId,StreamTo,Result])
                ;
              Err->  
                io:format("Download Err ~p~n",[Err])
                end,
                
            ok;
        Err->
            io:format("Message Err ~p~n",[Err]),
            error
    end
catch
_:_->ok end,
{ok, State};
%poll
handle_event({update,_BotName,#{
                    message:=#{
                            chat:=#{id:=ChatId},
                            poll:=Poll
                        }
                    }=_Message}, #{name:=Name}=State) ->
       #{question:=Question,options:=Options}=Poll,     
       OptionsNew=[#{text =>Text}||#{text := Text}<-Options],
        io:format("poll ~p~n",[Options]),                
       telegram_bot_api:sendPoll(Name,#{chat_id=>ChatId,question=>Question,options=>OptionsNew}),
    {ok, State};

handle_event({error,_BotName,_Status,_Json}, State) ->
    {ok, State};
handle_event(_Event, State) ->
    io:format("handle_event   ~p~n",[_Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, no_reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
