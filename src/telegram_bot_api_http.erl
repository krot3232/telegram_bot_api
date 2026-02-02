%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_http).

-define(CRLF, <<"\r\n">>).
-define(BOUNDARY, <<"--AaB03x01">>).

-export([init/1, terminate/1, raw/6, multipart/6, download/6]).
-export([async_receiver/2]).

-export_type([
    ipv4/0,
    module_name/0,
    http_proxy/0,
    http_option/0,
    option_request/0,
    http_timeout/0,
    http_endpoint/0
]).

-type 'StreamTo'() :: none | self | {self, once} | file:name_all().
-type 'BodyFormat'() :: string() | binary().

-type 'Receiver'() ::
    {telegram_bot_api_http, async_receiver, [pid]}
    | pid()
    | {ReceiverModule :: atom(), ReceiverFunction :: atom(), ReceiverArgs :: list()}
    | term().

-type ssl_ls_option() :: term().

-type 'HttpVersion'() :: string().
-type 'HttpOption'() ::
    {timeout, timeout()}
    | {connect_timeout, timeout()}
    | {ssl, [ssl_ls_option()]}
    | {autoredirect, boolean()}
    | {proxy_auth, {string(), string()}}
    | {version, 'HttpVersion'()}
    | {relaxed, boolean()}.
-type 'OptionRequest'() ::
    {sync, boolean()}
    | {stream, 'StreamTo'()}
    | {body_format, 'BodyFormat'()}
    | {full_result, boolean()}
    | {headers_as_is, boolean()}
    | {socket_opts, [term()]}
    | {receiver, 'Receiver'()}
    | {ipv6_host_with_brackets, boolean()}.

-type module_name() :: telegram_bot_api_http | atom().
-type http_option() :: nonempty_list('HttpOption'()).
-type option_request() :: nonempty_list('OptionRequest'()).
-type http_timeout() :: integer() | infinity.
-type http_endpoint() :: binary().
-type ipv4() :: {0..255, 0..255, 0..255, 0..255}.
-type http_port() :: pos_integer().
-type http_proxy() :: {string(), http_port()}.

-type state() ::
    #{
        token := binary(),
        http_module := module_name(),
        http_option := http_option(),
        option_request := nonempty_list('OptionRequest'()),
        http_endpoint := http_endpoint(),
        http_proxy => http_proxy(),
        _ => term()
    }.

-type reply_tag() ::
    reference()
    | nonempty_improper_list('alias', reference())
    | nonempty_improper_list(
        nonempty_improper_list('alias', reference()), term()
    ).
-type from() :: {Client :: pid(), Tag :: reply_tag()}.

-callback init(State :: state()) -> StateNew :: state().
-callback terminate(State :: state()) -> ok.
-callback raw(
    HttpEndpoint :: binary(),
    Method :: binary(),
    Data :: map(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result().
-callback multipart(
    HttpEndpoint :: binary(),
    Method :: binary(),
    Data :: map(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result().
-callback download(
    HttpEndpoint :: binary(),
    FilePath :: binary(),
    StreamTo :: telegram_bot_api_file:name_all(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result().

-spec init(State :: state()) -> StateNew :: state().
init(State) ->
    case State of
        #{http_proxy := HttpProxy} ->
            httpc:set_options([{proxy, {HttpProxy, []}}, {https_proxy, {HttpProxy, []}}]);
        _ ->
            ok
    end,
    State.

-spec terminate(State :: state()) -> ok.
terminate(_State) -> ok.

-type result() ::
    {ok, HttpCode :: integer(), Json :: map(), State :: state()}
    | {ok, Ref :: reference(), State :: state()}
    | {error, Err :: term(), State :: state()}.

-spec raw(
    HttpEndpoint :: binary(),
    Method :: binary(),
    Data :: map(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result().
raw(
    HttpEndpoint,
    Method,
    Data,
    Async,
    From,
    #{token := Token, http_option := HttpOption, option_request := OptionRequest} = State
) ->
    Body = telegram_bot_api_util:json_encode(Data),
    Url = <<HttpEndpoint/binary, "/bot", Token/binary, $/, Method/binary>>,
    case
        httpc:request(
            post,
            {Url, [], "application/json", Body},
            HttpOption,
            option_request(Async, From, OptionRequest)
        )
    of
        {ok, {{_HttpStatus, HttpCode, _HttpState}, _HttpHeaders, HttpBody}} ->
            {ok, HttpCode, telegram_bot_api_util:json_decode(HttpBody), State};
        {ok, Ref} ->
            {ok, Ref, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec multipart(
    HttpEndpoint :: binary(),
    Method :: binary(),
    Data :: map(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result().
multipart(
    HttpEndpoint,
    Method,
    Data,
    Async,
    From,
    #{token := Token, http_option := HttpOption, option_request := OptionRequest} = State
) ->
    Url = <<HttpEndpoint/binary, "/bot", Token/binary, $/, Method/binary>>,
    Type = binary_to_list(<<"multipart/form-data; boundary=", ?BOUNDARY/binary>>),
    Body = multipart_body(Data),
    case
        httpc:request(
            post,
            {Url, [{"Content-Length", integer_to_list(size(Body))}], Type, Body},
            HttpOption,
            option_request(Async, From, OptionRequest)
        )
    of
        {ok, {{_HttpStatus, HttpCode, _HttpState}, _HttpHeaders, HttpBody}} ->
            {ok, HttpCode, telegram_bot_api_util:json_decode(HttpBody), State};
        {ok, Ref} ->
            {ok, Ref, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec download(
    HttpEndpoint :: binary(),
    FilePath :: binary(),
    StreamTo :: telegram_bot_api_file:name_all(),
    Async :: boolean(),
    From :: from(),
    State :: state()
) -> result() | {ok, saved_to_file, state()}.
download(
    HttpEndpoint,
    FilePath,
    StreamTo,
    Async,
    From,
    #{token := Token, http_option := HttpOption, option_request := OptionRequest} = State
) ->
    Url = <<HttpEndpoint/binary, "/file/bot", Token/binary, $/, FilePath/binary>>,
    OptionRequest1 = [{stream, StreamTo} | option_request(Async, From, OptionRequest)],
    case
        httpc:request(
            get,
            {Url, []},
            HttpOption,
            OptionRequest1
        )
    of
        {ok, saved_to_file} ->
            {ok, saved_to_file, State};
        {ok, Ref} ->
            {ok, Ref, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

%%%%%%%%%%%%%%%%%%
async_receiver({Ref, saved_to_file}, Pid) ->
    Pid ! {async, Ref, saved_to_file};
async_receiver({Ref, {{_HttpStatus, HttpCode, _HttpState}, _HttpHeaders, HttpBody}}, Pid) ->
    Pid ! {async, Ref, {ok, HttpCode, telegram_bot_api_util:json_decode(HttpBody)}};
async_receiver(Msg, Pid) ->
    Pid ! {error, Msg}.

option_request(true, {Pid, _Tag} = _From, OptionRequest) ->
    OptionRequest ++ [{sync, false}, {receiver, {?MODULE, async_receiver, [Pid]}}];
option_request(_, _, OptionRequest) ->
    OptionRequest.

multipart_body(Data) ->
    I = maps:next(maps:iterator(Data)),
    multipart_body(I, [<<"--", ?BOUNDARY/binary, "--", ?CRLF/binary, ?CRLF/binary>>]).

multipart_body(none, Acc) ->
    list_to_binary(Acc);
multipart_body({K, V, INext}, Acc) ->
    Var = atom_to_binary(K),
    Name =
        case V of
            #{name := FileName, file := File} ->
                {ok, Bin} = file:read_file(File),
                <<Var/binary, "\"; filename=\"", FileName/binary, "\"", ?CRLF/binary, ?CRLF/binary,
                    Bin/binary>>;
            _ ->
                V2 = telegram_bot_api_util:to_binary(V),
                <<Var/binary, "\"", ?CRLF/binary, ?CRLF/binary, V2/binary>>
        end,
    Body =
        <<"--", ?BOUNDARY/binary, ?CRLF/binary, "Content-Disposition: form-data; name=\"",
            Name/binary, ?CRLF/binary>>,
    multipart_body(maps:next(INext), [Body | Acc]).
