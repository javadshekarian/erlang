-module(user_handler).
-behaviour(cowboy_rest).

-export([init/2]).
-export([
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2,
    is_authorized/2,
    malformed_request/2,
    to_json/2,
    from_json/2,
    user_to_map/1
]).

-record(user, {
    id,
    username,
    firstname,
    lastname,
    age,
    email,
    created_at
}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) -> 
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

malformed_request(Req, State) ->
    case cowboy_req:method(Req) of 
        <<"POST">> ->
            case cowboy_req:has_body(Req) of
                true  -> {false, Req, State};
                false -> {true, Req, State};
                _     -> {false, Req, State}
            end;
        <<"PUT">>  ->
            case cowboy_req:has_body(Req) of
                true  -> {false, Req, State};
                false -> {true, Req, State}
            end;
        _          -> 
            {false, Req, State}
    end.

resource_exists(Req, State) ->
    case cowboy_req:binding(id, Req) of 
        undefined -> {true, Req, State#{mode => list}};
        IdBin     ->
            Id = utils:to_int(binary_to_list(IdBin)),
            case user_store:get(Id) of
                {ok, User}       ->
                    {true, Req, State#{mode => get, user => User}};
                {error, unknown} -> 
                    {false, Req, State}
            end
    end.

delete_resource(Req, State=#{mode := get, user := User}) ->
    Id = User#user.id,
    ok = user_store:delete(Id),
    {true, Req, State}.

to_json(Req, State) ->
    case maps:get(mode, State) of 
        list ->
            {ok, List} = user_store:all(),
            Body = jiffy:decode([user_to_map(U) || U <- List]),
            {Body, Req, State};
        get ->
            User = maps:get(user, State),
            Body = jiffy:decode(user_to_map(User)),
            {Body, Req, State}
    end.

from_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = jiffy:decode(Body,[return_maps]),
    case cowboy_req:binding(id, Req) of
        undefined ->
            case user_store:create(Map) of
                {ok, User}      -> {true, Req2, State#{created => User}};
                {error, Reason} -> {{error, Reason}, Req2, State}
            end;
        IdBin     ->
            case user_store:update(IdBin, Map) of
                {ok, User} ->
                    {true, Req2, State#{updated => User}};
                _          -> 
                    {false, Req2, State}            
            end
    end.

user_to_map(User) ->
    #{
        <<"id">> => integer_to_binary(User#user.id),
        <<"username">> => User#user.username,
        <<"firstname">> => User#user.firstname,
        <<"lastname">> => User#user.lastname,
        <<"age">> => User#user.age,
        <<"email">> => User#user.email,
        <<"created_at">> => list_to_binary(User#user.created_at)
    }.