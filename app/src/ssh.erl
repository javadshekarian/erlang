-module(ssh).
-bahaviour(gen_statem).

-export([start_link/0, toggle/0, get_count/0, call_info/0]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).


toggle() -> 
    gen_statem:cast(?MODULE, toggle).

get_count() -> 
    gen_statem:call(?MODULE, get_count).

start_link() -> 
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() -> 
    handle_event_function.

call_info() -> 
    gen_statem:call(?MODULE, something).

init([]) ->
    {ok, off, #{state => off, count => 0}}.

handle_event(cast, toggle, State, Data) -> 
    case State of 
        off -> 
            io:format("Current State Is off And Count Value Is: ~p~n", [maps:get(count, Data)]),
            NewData = Data#{state => on, count => maps:get(count, Data) +1},
            {next_state, on, NewData};
        on ->
            io:format("Current State Is on And Count Value Is: ~p~n", [maps:get(count, Data)]),
            NewData = Data#{state => off, count => maps:get(count, Data) +1},
            {next_state, off, NewData}
    end;

handle_event(cast, on, _State, Data) ->
    io:format("Current State Is off And Count Value Is: ~p~n", [maps:get(count, Data)]),
    NewData = Data#{state => off, count => maps:get(count, Data) + 1},
    {next_state, off, NewData};

handle_event(cast, off, _State, Data) -> 
    io:format("Current State Is on And Count Value Is: ~p~n", [maps:get(count, Data)]),
    NewData = Data#{state => on, count => maps:get(count, Data) + 1},
    {next_state, on, NewData};

handle_event({call, From}, get_count, _State, Data) ->
    {keep_state, Data, {reply, From, Data}};

handle_event({call, From}, _Event, State, Data) -> 
    io:format("Current State: ~p, Current Data: ~p~n", [State, Data]),
    {keep_state, Data, {reply, From, Data}}.

terminate(_Reason, _State, _Extra) -> ok.