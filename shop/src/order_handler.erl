-module(order_handler).
-export([init/2, order_to_map/1]).

-record(order, {
    id,
    username,
    fk_product_id,
    created_at,
    order_type
}).

order_to_map(#order{created_at = CreatedAt, fk_product_id = FkProductId, id = Id, order_type = OrderType, username = Username}) -> 
    % io:format("~p~n~p~n~p~n~p~n~p~n", [CreatedAt, fk])
    #{
        <<"id">> => integer_to_binary(Id),
        <<"username">> => Username,
        <<"order_type">> => OrderType,
        <<"fk_product_id">> => FkProductId,
        <<"created_at">> => list_to_binary(CreatedAt)
    }.

init(Req, _State) ->
    Method = cowboy_req:method(Req),
    case {Method, cowboy_req:binding(id, Req)} of
        {<<"GET">>, undefined}  -> handle_list(Req);
        {<<"POST">>, undefined} -> handle_create(Req);
        {<<"GET">>, IdBin}      -> handle_get(Req, IdBin);
        {<<"PUT">>, IdBin}      -> handle_update(Req, IdBin);
        {<<"DELETE">>, IdBin}   -> handle_delete(Req, IdBin);
        _ ->
            Req2 = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>},
                                   jiffy:encode(#{error => <<"method_not_allowed">>}), Req),
            {ok, Req2, undefined}
    end.

handle_list(Req) ->
    case order_store:all() of
        {ok, List} ->
            Req2 = cowboy_req:reply(
                200, 
                #{<<"content-type">> => <<"application/json">>}, 
                jiffy:encode([order_to_map(Item) || Item <- List]), 
                Req
            ),
            {ok, Req2, undefined};
        {error, _} ->
            Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"internal">>}), Req),
            {ok, Req2, undefined}
    end.

handle_create(Req) ->
    {ok, BodyBin, Req2} = cowboy_req:read_body(Req),
    case catch jiffy:decode(BodyBin, [return_maps]) of
        {'EXIT', _} ->
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"invalid_json">>}), Req2),
            {ok, Req3, undefined};
        Map ->
            case order_store:create(Map) of
                {ok, Order} ->
                    io:format("~p~n~p~n",[Order, order_to_map(Order)]),
                    Req3 = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(order_to_map(Order)), Req2),
                    {ok, Req3, undefined};
                {error, invalid_fk} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"invalid_fk">>}), Req2),
                    {ok, Req3, undefined};
                {error, fk_not_found} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"fk_not_found">>}), Req2),
                    {ok, Req3, undefined};
                {error, Reason} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => Reason}), Req2),
                    {ok, Req3, undefined}
            end
    end.

handle_get(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    case order_store:get(Id) of
        {ok, Order} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">>=><<"application/json">>}, jiffy:encode(order_to_map(Order)), Req),
            {ok, Req2, undefined};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"not_found">>}), Req),
            {ok, Req2, undefined}
    end.

handle_update(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    {ok, BodyBin, Req2} = cowboy_req:read_body(Req),
    case catch jiffy:decode(BodyBin, [return_maps]) of
        {'EXIT', _} ->
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"invalid_json">>}), Req2),
            {ok, Req3, undefined};
        Map ->
            case order_store:update(Id, Map) of
                {ok, Order} ->
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(order_to_map(Order)), Req2),
                    {ok, Req3, undefined};
                {error, not_found} ->
                    Req3 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Req2),
                    {ok, Req3, undefined};
                {error, fk_not_found} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"fk_not_found">>}), Req2),
                    {ok, Req3, undefined}
            end
    end.

handle_delete(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    ok = order_store:delete(Id),
    Req2 = cowboy_req:reply(204, #{}, <<>> , Req),
    {ok, Req2, undefined}.
