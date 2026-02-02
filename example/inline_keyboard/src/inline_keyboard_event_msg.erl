-module(inline_keyboard_event_msg).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/message_effect.hrl").

init([Args]) ->

    {ok, Args#{inc=>#{}}}.

 
handle_event({update,_BotName,#{ message:=#{chat:=#{id:=ChatId},text:=_Text}}}, #{name:=Name,inc:=Inc}=State) ->
    IncBin=integer_to_binary(maps:get(ChatId,Inc,0)),
   telegram_bot_api:sendMessage(Name,#{
                            chat_id=>ChatId,
                            text=><<"inline_keyboard ",IncBin/binary>>,
                            reply_markup=>
                                    #{
                                    inline_keyboard=>
                                    [
                                    [
                                    #{
                                        text=><<"next +1">>,
                                        callback_data=><<"callback_1">>
                                    },
                                    #{
                                        text=><<"next +2">>,
                                        callback_data=><<"callback_2">>
                                    }
                                    ]
                                    ]
                                }
                            }),    
    {ok, State};

handle_event({update,_BotName,#{
                callback_query:=#{
                 data := Callback,
                 message:=#{chat:=#{id:=ChatId}}
                 }
                }
                 =_Message}, #{name:=Name,inc:=Inc}=State) ->
        [_|IB]=binary:split(Callback,[<<$_>>],[]),
        I=binary_to_integer(hd(IB)),
        IncNew = maps:update_with(ChatId,fun(V) -> V + I end,I,Inc),
        io:format("Chat: ~p ~p~n",[ChatId,IncNew]),
        IncBin=integer_to_binary(maps:get(ChatId,IncNew)),

        A =integer_to_binary(rand:uniform(99)),
        B =integer_to_binary(rand:uniform(99)),
         telegram_bot_api:sendMessage(Name,#{
                                  chat_id=>ChatId,
                                  text=><<"inline_keyboard ",IncBin/binary>>,
                                  reply_markup=>
                                          #{
                                          inline_keyboard=>
                                          [
                                          [
                                          #{
                                              text=><<"next +",A/binary>>,
                                              callback_data=><<"callback_",A/binary>>
                                          },
                                          #{
                                              text=><<"next +",B/binary>>,
                                              callback_data=><<"callback_",B/binary>>
                                          }
                                          ]
                                          ]
                                      }
                                  }),    
          {ok, State#{inc=>IncNew}};
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
