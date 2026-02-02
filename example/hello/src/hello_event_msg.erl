-module(hello_event_msg).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/message_effect.hrl").

init([Args]) ->
    {ok, Args}.


handle_event({update,BotName,#{
                    message:=#{
                            chat:=#{id:=ChatId},
                            text:=<<"hello">>
                        }
                    }=_Message}, #{name:=Name}=State) ->
                 io:format("BotName ~p~n",[BotName]),       
   telegram_bot_api:sendMessage(Name,#{
                                        chat_id=>ChatId,
                                        text=><<"world">>,
                                        message_effect_id=>?MESSAGE_EFFECT_PARTY_POPPER
                                        }),

    {ok, State};
handle_event({update,_BotName,#{
                    message:=#{
                            chat:=#{id:=ChatId},
                            text:=Text
                        }
                    }=_Message}, #{name:=Name}=State) ->
                        io:format("handle_event update ~ts~n",[Text]),
    telegram_bot_api:sendMessage(Name,#{
                            chat_id=>ChatId,
                            text=><<"Error! write <code>hello</code>">>,
                            parse_mode=><<"html">>
                            }),
    {ok, State};
handle_event({error,_BotName,_Status,_Json}, State) ->
    io:format("handle_event error ~p ~p ~n",[_Status,_Json]),
    {ok, State};
handle_event(_Event, State) ->
    io:format("handle_event ~p~n",[_Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, no_reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
