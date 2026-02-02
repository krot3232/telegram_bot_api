%%%-------------------------------------------------------------------
%% @doc inline_keyboard top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(inline_keyboard_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 100,
        period => 60
    },
    ChildSpecs = [
        #{
        id => inline_keyboard_event,
        start => {gen_event, start_link, [{local, inline_keyboard_event}]},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [dynamic]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
