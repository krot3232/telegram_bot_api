%%% @author Konstantin Rusalov
%%% @copyright (c) 2026 Konstantin Rusalov
-module(telegram_bot_api_file).
-export([download/3, download/4]).

-type deep_list() :: [char() | atom() | deep_list()].
-type name_all() :: string() | atom() | deep_list() | (RawFilename :: binary()).

-export_type([name_all/0]).

-spec download(
    Pool :: telegram_bot_api:pool_name(),
    FilePath :: binary(),
    StreamTo :: name_all(),
    Async :: boolean()
) ->
    Result :: saved_to_file | telegram_bot_api:ret().
download(Pool, FilePath, StreamTo, Async) ->
    wpool:call(Pool, {download, FilePath, StreamTo, Async}).

-spec download(Pool :: telegram_bot_api:pool_name(), FilePath :: binary(), StreamTo :: name_all()) ->
    Result :: saved_to_file | telegram_bot_api:ret().
download(Pool, FilePath, StreamTo) -> download(Pool, FilePath, StreamTo, false).
