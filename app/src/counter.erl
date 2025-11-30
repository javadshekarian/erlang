-module(counter).
-behaviour(gen_server).

-export([start_link/0, inc/0, get/0, stop/0, send_info/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
start_link() ->
    SpawnOpt = [
        link,
        {priority, high},
        {fullsweep_after, 10}
    ],

    gen_server:start_link({local, ?MODULE}, ?MODULE, 0, [
        {debug, [log, trace, debug, statistics]},
        {timeout, 5000},
        {hibernate_after, 2000},
        {spawn_opt, SpawnOpt}
    ]).

inc() ->
    gen_server:cast(?MODULE, inc).

get() ->
    gen_server:call(?MODULE, get).

stop() ->
    gen_server:stop(?MODULE).

send_info(Msg) ->
    ?MODULE ! Msg.


init(Initial) ->
    % timer:sleep(5000),
    io:format("Initial value = ~p~n", [Initial]),
    {ok, Initial}.

handle_cast(inc, State) ->
    NewState = State + 1,
    io:format("[cast] increment → ~p~n", [NewState]),
    {noreply, NewState}.

handle_call(get, From, State) ->
    io:format("[call] get from ~p, current state = ~p~n", [From, State]),
    {reply, State, State}.

handle_info(Msg, State) ->
    io:format("[info] received: ~p, current state = ~p~n", [Msg, State]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("[terminate] reason: ~p, final state: ~p~n",
              [Reason, State]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
