%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_util).
-export([json_encode/1, json_decode/1]).
-export([to_binary/1]).
-export([get_ip/0, get_ip/1]).

-spec json_encode(Map :: dynamic()) -> binary().
json_encode(Map) ->
    iolist_to_binary(json:encode(Map)).

-spec json_decode(Bin :: binary()) -> map().
json_decode(Bin) ->
    {Map, ok, <<>>} = json:decode(Bin, ok, #{
        object_push => fun(Key1, Value1, Acc1) -> [{binary_to_atom(Key1), Value1} | Acc1] end
    }),
    Map.

-doc false.
-spec to_binary(V :: term()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> telegram_bot_api_util:json_encode(V);
to_binary(V) when is_atom(V) -> list_to_binary(atom_to_list(V));
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_map(V) -> json_encode(V);
to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).

-doc """
Request a public IP address via site http://ifconfig.me/ip
""".
-spec get_ip() -> {ok, binary()} | {error, term()}.
get_ip() ->
    Url = "http://ifconfig.me/ip",
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Get ip address
## Examples:
```erlang
    {ok,<<"1.1.1.1">>}=telegram_bot_api_util:get_ip("ens3").
```
""".
-spec get_ip(InterfaceName :: string()) -> {ok, binary()} | {error, term()}.
get_ip(InterfaceName) ->
    case inet:getifaddrs() of
        {ok, L} ->
            case proplists:get_value(addr, proplists:get_value(InterfaceName, L, []), []) of
                {_, _, _, _} = Ip -> {ok, list_to_binary(inet:ntoa(Ip))};
                Reason -> {error, Reason}
            end;
        Reason ->
            {error, Reason}
    end.
