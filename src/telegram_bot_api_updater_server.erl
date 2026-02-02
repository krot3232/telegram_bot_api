%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_updater_server).
-behaviour(gen_server).

-moduledoc """
Use this gen_server to receive incoming updates using long polling.

See `m:telegram_bot_api#long-polling`
""".

-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([name_server/1, child_spec/1]).

-define(TIME, 3000).

-type child_id() :: term().

stop(Pid) ->
    gen_server:stop(Pid).

-spec start_link(
    Op :: #{
        id := child_id(),
        name := Pool :: telegram_bot_api:pool_name(),
        update_time := UpdateTime :: integer(),
        offset := Offset :: integer(),
        event := Event :: telegram_bot_api_app:event(),
        limit => Limit :: integer(),
        allowed_updates => AllowedUpdates :: nonempty_list(telegram_bot_api:update_type())
    }
) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(#{id := Id} = Opts) ->
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).

init(Opts) ->
    {ok, Opts#{
        timer => erlang:send_after(1, self(), update),
        update_time => maps:get(update_time, Opts, ?TIME),
        offset => maps:get(offset, Opts, 0)
    }}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    update,
    #{timer := OldTimer, update_time := UpdateTime, offset := Offset, name := Name, event := Event} =
        State
) ->
    erlang:cancel_timer(OldTimer),
    Param0 = #{
        offset => Offset,
        limit => maps:get(limit, State, 100)
    },
    Param =
        case maps:get(allowed_updates, State, undef) of
            undef -> Param0;
            A -> Param0#{allowed_updates => A}
        end,
    OffsetNew =
        try telegram_bot_api:getUpdates(Name, Param) of
            {ok, 200, #{result := []}} ->
                Offset;
            {ok, 200, #{result := Result} = _Json} ->
                lists:foldl(
                    fun
                        (#{update_id := Id} = E, Acc) when Id > Acc ->
                            gen_event:notify(Event, {update, Name, E}),
                            Id;
                        (E, Acc) ->
                            gen_event:notify(Event, {update, Name, E}),
                            Acc
                    end,
                    0,
                    Result
                ) + 1;
            {ok, Status, Json} when is_map(Json) ->
                gen_event:notify(Event, {error, Name, status, {Status, Json}}),
                Offset;
            _ ->
                Offset
        catch
            E:M ->
                gen_event:notify(Event, {error, Name, E, M}),
                Offset
        end,
    {noreply, State#{timer => erlang:send_after(UpdateTime, self(), update), offset => OffsetNew}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

name_server(BotName) ->
    binary_to_atom(list_to_binary(io_lib:format("~p_updater_server", [BotName]))).

child_spec(
    #{
        name := Pool,
        update_time := _UpdateTime,
        offset := _Offset,
        event := _E
    } = Op
) ->
    Id =
        case maps:get(id, Op, undef) of
            Id1 when is_atom(Id1), Id1 =/= undef -> Id1;
            _ -> name_server(Pool)
        end,
    #{
        id => Id,
        start => {telegram_bot_api_updater_server, start_link, [Op#{id => Id}]},
        restart => transient,
        shutdown => brutal_kill,
        type => worker,
        modules => [telegram_bot_api_webhook_server]
    }.
