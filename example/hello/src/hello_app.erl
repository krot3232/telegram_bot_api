%%%-------------------------------------------------------------------
%% @doc hello public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok,Pid}=hello_sup:start_link(),

    {ok,Token}=application:get_env(hello, token),
    {ok,Name}=application:get_env(hello, name),

   
    gen_event:add_handler(hello_event, hello_event_msg, [#{name=>Name,token=>Token}]),

    {ok, _Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>Name,token=>Token,
        workers=>1
      }),
      {ok, _Pid2} =  telegram_bot_api_sup:start_update(#{
          name=>Name,  
          update_time=>800,
          offset=>0,
          limit=>100,
          allowed_updates=>[message, edited_channel_post, callback_query],
          event=>hello_event
    }),
 
    %%%%%%% bot 2


% Token2= <<"144552:AAAAAAAAAAAAAAAAAAAAAAAAAAk">>,
% Name2=bot2,
% gen_event:add_handler(hello_event, hello_event_msg, [#{name=>Name2,token=>Token2}]),

%   telegram_bot_api_sup:start_pool(#{
%         name=>Name2,token=>Token2,
%         workers=>1
%       }),
%      telegram_bot_api_sup:start_update(#{
%           name=>Name2,  
%           update_time=>800,
%           offset=>0,
%           limit=>100,
%           allowed_updates=>[message, edited_channel_post, callback_query],
%           event=>hello_event
%     }),

 

    {ok,Pid}.


stop(_State) ->
    ok.

%% internal functions
