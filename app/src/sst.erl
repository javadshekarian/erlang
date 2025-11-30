-module(sst).
-behaviour(gen_statem).

-export([start_link/0, get_count/0, send_info/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

get_count() -> 
    gen_statem:call(?MODULE, get_count).

start_link() -> 
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() -> 
    handle_event_function.

send_info(Msg) -> 
    sst ! {Msg}.

init([]) ->
    io:format("Traffic Light Started: Red~n"),
    {ok, red, #{count => 0, state => red, fullname => "javadshekarian"}, 4000}.

handle_event(timeout, _TimeoutInfo, red, Data) -> 
    io:format("_TimeoutInfo: ~p~n", [_TimeoutInfo]),
    io:format("Red -> Green~n"),
    NewState = Data#{state => green, count => maps:get(count, Data) + 1},
    {next_state, green, NewState, 4000};

handle_event(timeout, _TimeoutInfo, green, Data) ->
    io:format("_TimeoutInfo: ~p~n", [_TimeoutInfo]),
    io:format("Green -> Yellow~n"),
    NewState = Data#{state => yellow, count => maps:get(count, Data) +1},
    {next_state, yellow, NewState, 4000};

handle_event(timeout, _TimeoutInfo, yellow, Data) ->
    io:format("_TimeoutInfo: ~p~n", [_TimeoutInfo]),
    io:format("Yellow -> Red~n"),
    NewState = Data#{state => red, count => maps:get(count, Data) + 1},
    {next_state, red, NewState, 4000};

handle_event(info, Msg, State, Data) ->
    io:format("Received unexpected message: ~p in state ~p~n", [Msg, State]),
    {next_state, red, Data, 4000};

handle_event(_Type, _Event, _State, Data) ->
    {keep_state, Data}.

terminate(_Reason, _State, _Data) ->
    io:format("Traffic Light stopped.~n"),
    ok.