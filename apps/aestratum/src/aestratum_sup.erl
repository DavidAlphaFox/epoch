-module(aestratum_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 5, 10}, Procs}}.

