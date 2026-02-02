%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_server).
-behaviour(gen_server).

-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HTTP_TIMEOUT, 10000).
-define(HTTP_MODULE, telegram_bot_api_http).
-define(HTTP_ENDPOINT, <<"https://api.telegram.org">>).

stop(Pid) ->
    gen_server:stop(Pid).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

init([Opts]) when is_map(Opts) ->
    HttpModule = maps:get(http_module, Opts, ?HTTP_MODULE),
    State = Opts#{
        http_module => HttpModule,
        http_option => maps:get(http_option, Opts, [
            {timeout, maps:get(http_timeout, Opts, ?HTTP_TIMEOUT)}, {ssl, [{verify, verify_none}]}
        ]),
        option_request => maps:get(option_request, Opts, [{body_format, binary}]),
        http_endpoint => maps:get(http_endpoint, Opts, ?HTTP_ENDPOINT)
    },
    {ok, HttpModule:init(State)};
init(_) ->
    {stop, {error, nomatch}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(
    {Type, Method, Data, Async},
    From,
    #{http_module := HttpModule, http_endpoint := HttpEndpoint} = State
) when Type =:= raw; Type =:= multipart; Type =:= download ->
    try HttpModule:Type(HttpEndpoint, Method, Data, Async, From, State) of
        {ok, HttpCode, JsonMap, StateNew} ->
            {reply, {ok, HttpCode, JsonMap}, StateNew};
        {ok, Ref, StateNew} when is_reference(Ref) ->
            {reply, {ok, Ref}, StateNew};
        {ok, saved_to_file, StateNew} ->
            {reply, saved_to_file, StateNew};
        {error, Er, StateNew} ->
            {reply, {error, Er}, StateNew}
    catch
        E:M ->
            {reply, {E, M}, State}
    end;
handle_call(Msg, _From, State) ->
    {reply, {error, {nomatch, Msg}}, State}.

handle_cast({set_http_option, Type, Val}, #{http_option := HttpOption} = State) ->
    HttpOptionNew = [{Type, Val} | proplists:delete(Type, HttpOption)],
    {noreply, State#{http_option => HttpOptionNew}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) when is_map(State) ->
    HttpModule = maps:get(http_module, State, ?HTTP_MODULE),
    HttpModule:terminate(State);
terminate(_Reason, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
