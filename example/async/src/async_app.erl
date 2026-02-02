%%%-------------------------------------------------------------------
%% @doc async public API
%% @end
%%%-------------------------------------------------------------------

-module(async_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    {ok,Pid}=    async_sup:start_link(),

    
    {ok,Token}=application:get_env(async, token),
    {ok,Name}=application:get_env(async, name),

  
    gen_event:add_handler(async_event, async_event_msg, [#{name=>Name,token=>Token}]),

    {ok, _Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>Name,
        token=>Token,
        workers=>3
      }),
    {ok, _Pid2} = telegram_bot_api_sup:start_update(#{
          name=>Name,  
          update_time=>1000,
          offset=>0,
          limit=>100,
          event=>async_event,
          allowed_updates=>[message,message]
    }),
  
    {ok,Pid}.

stop(_State) ->
    ok.

%% internal functions
