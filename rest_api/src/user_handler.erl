-module(user_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),

    Reply = 
        case Method of
            % create user 
            <<"POST">> -> 
                {ok, Body, _} = cowboy_req:read_body(Req),
                Data = jsx:decode(Body),
                #{<<"id">> := Id, <<"username">> := Username} = Data,
                user_state:add({Id, binary_to_list(Username)}),
                #{message => <<"created">>};

            % get all or get one
            <<"GET">> -> 
                case cowboy_req:binding(id, Req) of
                    undefined -> 
                        #{users => user_state:all()};
                    IdStr ->
                        Id = list_to_integer(binary_to_list(IdStr)),
                        case user_state:get_one(Id) of
                            undefined -> 
                                #{error => <<"User not found">>};
                            User ->
                                #{user => User}
                        end
                end;

            % delete user 
            <<"DELETE">> -> 
                case cowboy_req:binding(id, Req) of 
                    undefined -> 
                        #{error => <<"Id Is Required!">>};
                    IdStr -> 
                        Id = list_to_integer(binary_to_list(IdStr)),
                        case user_state:delete(Id) of
                            ok -> 
                                #{message => <<"User deleted">>};
                            {error, not_found} -> 
                                #{error => <<"User not found">>};
                            _ -> 
                                #{error => <<"Delete failed">>}
                        end
                end;

            _ -> 
                #{error => <<"Method Not Allowed!">>}
        end,
    
    {ok, cowboy_req:reply(
        200,
        #{<<"Content-Type">> => <<"application/json">>},
        jsx:encode(Reply),
        Req
    ), State}.