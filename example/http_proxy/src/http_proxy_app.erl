%%%-------------------------------------------------------------------
%% @doc http_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(http_proxy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Pid}= http_proxy_sup:start_link(),


    Token= <<"1111111111:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
    Name = http_proxy_bot,
 
    ChatId= <<"@chat123">>,

    HttpProxy={"127.0.0.1",8118},
    {ok, _Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>Name,token=>Token,
        workers=>1,
        http_proxy=>HttpProxy
      }),

    I=integer_to_binary(rand:uniform(8888)),
    Result=telegram_bot_api:sendMessage(Name,#{
      chat_id=>ChatId,
      text=><<"Message",I/binary>>
    }),
    io:format("Result ~p~n",[Result]),


    {ok,Pid}.

stop(_State) ->
    ok.

%% internal functions
