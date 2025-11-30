-module(user_state).
-export([start/0, add/1, get_one/1, all/0, delete/1]).

start() -> register(user_state, spawn(fun loop/0)).

loop() -> 
    receive 
        {add, User, From} -> 
            put(users, [User | get_state()]),
            From ! ok,
            loop();

        {all, From} ->
            From ! get_state(),
            loop();

        {get_one, Id, From} -> 
            U = lists:keyfind(Id, 1, get_state()),
            From ! U,
            loop();

        {delete, Id, From} ->
            put(users, lists:keydelete(Id, 1, get_state())),
            From ! ok,
            loop()
    end.

get_state() -> 
    case get(users) of 
        undefined -> [];
        S -> S
    end.

% wrappers 
add(U) -> user_state ! {add, U, self()}, receive X -> X end.
all() -> user_state ! {all, self()}, receive X -> X end.
get_one(Id) -> user_state ! {get_one, Id, self()}, receive X -> X end.
delete(Id) -> user_state ! {delete, Id, self()}, receive X -> X end.
