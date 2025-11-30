-module(rest_api_router).
-export([dispatch/0]).

dispatch() ->
    cowboy_router:compile([
        {'_',[
            {"/user/:id", user_handler, []},
            {"/user", user_handler, []}
        ]}
    ]).