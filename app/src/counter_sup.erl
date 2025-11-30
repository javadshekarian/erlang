-module(counter_sup).
-behavior(supervisor).

% My Api 
-export([start_link/0]).

% Erlang Callbacks 
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> 
    RestartStrategy = one_for_one,
    MaxRestart = 5,
    MaxTime = 10,

    Flags = {RestartStrategy, MaxRestart, MaxTime},
    Child = #{
        id => counter                                        ,
        start => {counter, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        module => [counter]
    },
    
    {ok, {Flags, [Child]}}.