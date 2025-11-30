-module(rest_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    user_state:start(),

    Dispatch = rest_api_router:dispatch(),

    cowboy:start_clear(
        my_http,
        [{port, 8089}],
        #{env => #{dispatch => Dispatch}}
    ).

stop(_State) ->
    ok.