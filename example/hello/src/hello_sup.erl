%%%-------------------------------------------------------------------
%% @doc hello top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hello_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 100,
        period => 60
    },
    ChildSpecs = [
        #{
        id => hello_event,
        start => {gen_event, start_link, [{local, hello_event}]},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [dynamic]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

