%%%-------------------------------------------------------------------
%% @doc echo public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Pid}=echo_sup:start_link(),

    {ok,Token}=application:get_env(echo, token),
    {ok,Name}=application:get_env(echo, name),

 

    gen_event:add_handler(echo_event, echo_event_msg, [#{name=>Name,token=>Token}]),

    {ok, _Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>Name,
        token=>Token,
        workers=>erlang:system_info(schedulers_online)
      }),
    {ok, _Pid2} = telegram_bot_api_sup:start_update(#{
          name=>Name,
          update_time=>1000,
          offset=>0,
          limit=>100,
          allowed_updates=>[message,callback_query,channel_post],
          event=>echo_event
    }),
  

    {ok,Pid}.

stop(_State) ->
    ok.

%% internal functions
