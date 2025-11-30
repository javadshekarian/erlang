-module(websocket_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(users, [named_table, public, {read_concurrency, true}]),

    %% Setup Cowboy:
    Dispatch = cowboy_router:compile([
        {'_',[
            {"/ws", websocket_handler, []}
        ]}
    ]),

    case cowboy:start_clear(
        my_http_listener,
        [{port, 8082}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _Pid} -> 
            io:format("Websocket Is Listening To Port 8082~n"),
            {ok, self()};
        {error, Reason} -> 
            io:format("Cowboy Is Faild To Start. Reason: ~p~n",[Reason]),
            {stop, Reason}
    end.

stop(_State) ->
    ok.

