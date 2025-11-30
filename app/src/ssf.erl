-module(ssf).
-behaviour(gen_statem).

-export([start_link/0, toggle/0, get_count/0, stop/0]).

-export([init/1, callback_mode/0, terminate/3]).
-export([off/3, on/3]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

toggle() ->
    gen_statem:cast(?MODULE, toggle).

get_count() ->
    gen_statem:call(?MODULE, get_count).

% stop() ->
%     gen_statem:stop(?MODULE).
stop() -> 
    gen_statem:call(?MODULE, stop).

callback_mode() ->
    state_functions.

init([]) ->
    {ok, off, #{count => 0}}.

off(cast, toggle, Data=#{count := C}) ->
    io:format("Switching: OFF → ON~n"),
    NewData = Data#{count := C + 1},
    {next_state, on, NewData};

off({call, From}, get_count, Data) ->
    io:format("Current Running State Is off"),
    {keep_state, Data, {reply, From, Data}};

off({call, _From}, stop, Data) -> 
    io:format("Stopping Statem Server ...~n, Current Data: ~p~n",[Data]),
    {stop, normal, Data};

off(_Type, _Event, Data) ->
    {keep_state, Data}.

on(cast, toggle, Data=#{count := C}) ->
    io:format("Switching: ON → OFF~n"),
    NewData = Data#{count := C + 1},
    {next_state, off, NewData};

on({call, From}, get_count, Data) ->
    io:format("Current Running State Is on"),
    {keep_state, Data, {reply, From, Data}};

on({call, _From}, stop, Data) -> 
    io:format("Stopping Statem Server ...~n, Current Data: ~p~n",[Data]),
    {stop, normal, Data};

on(_Type, _Event, Data) ->
    {keep_state, Data}.

terminate(_Reason, _State, _Data) ->
    ok.
