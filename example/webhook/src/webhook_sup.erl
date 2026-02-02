%%%-------------------------------------------------------------------
%% @doc webhook top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(webhook_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).
-export([start_child/1]).
 
-define(SERVER, ?MODULE).

start_link(L) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [L]).


init([L]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 60
    },
    ChildSpecs1 = case L of
      [] ->[];
       _-> 
         [
            begin
                #{
                    id => Id,
                    start => {gen_event, start_link, [{global, Id}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [dynamic]
                }
        end
    ||Id<-L
    ]
    end,
    {ok, {SupFlags, ChildSpecs1}}.

%% internal functions
start_child(Id)->
Spec = #{
    id => Id,
    start => {gen_event, start_link, [{global, Id}]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [dynamic]
},
supervisor:start_child(?SERVER, Spec).