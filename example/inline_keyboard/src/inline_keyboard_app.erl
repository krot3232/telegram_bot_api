%%%-------------------------------------------------------------------
%% @doc inline_keyboard public API
%% @end
%%%-------------------------------------------------------------------

-module(inline_keyboard_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Pid}=inline_keyboard_sup:start_link(),
    {ok,Token}=application:get_env(inline_keyboard, token),
    {ok,Name}=application:get_env(inline_keyboard, name),
  
    gen_event:add_handler(inline_keyboard_event,inline_keyboard_event_msg, [#{name=>Name,token=>Token}]),
    {ok, _Pid1} = telegram_bot_api_sup:start_pool(#{
        name=>Name,token=>Token,
        workers=>3
    }),
    {ok, _Pid2} = telegram_bot_api_sup:start_update(#{
        name=>Name,
        update_time=>500,
        offset=>0,
        limit=>100,
        event=>inline_keyboard_event
    }),
    {ok,Pid}.

stop(_State) ->
    ok.