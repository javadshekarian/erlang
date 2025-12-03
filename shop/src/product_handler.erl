-module(product_handler).
-export([init/2, product_to_map/1]).

-include_lib("kernel/include/logger.hrl").

-record(product, {
    id,
    productName,
    created_at,
    explain,
    mark
}).

product_to_map(#product{id = Id, explain = Explain, mark = Mark, productName = ProductName, created_at = CreatedAt}) -> 
    #{
        <<"id">> => integer_to_binary(Id),
        <<"explain">> => Explain,
        <<"mark">> => Mark,
        <<"created_at">> => list_to_binary(CreatedAt),
        <<"productName">> => ProductName
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

%%% helpers %%%
handle_list(Req) ->
    case product_store:all() of
        {ok, List} ->
            MapList = [product_to_map(Map) || Map <- List],
            %% فقط برای دیباگ
            io:format("Product list: ~p~n", [MapList]),
            Body = jiffy:encode(MapList),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
            {ok, Req2, undefined};
        {error, _} ->
            Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>},
                                   jiffy:encode(#{error => <<"internal">>}), Req),
            {ok, Req2, undefined}
    end.

handle_create(Req) ->
    {ok, BodyBin, Req2} = cowboy_req:read_body(Req),
    case catch jiffy:decode(BodyBin, [return_maps]) of
        {'EXIT', _} ->
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                    jiffy:encode(#{error => <<"invalid_json">>}), Req2),
            {ok, Req3, undefined};
        Map ->
            case product_store:create(Map) of
                {ok, Product} ->
                    Req3 = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>},
                                            jiffy:encode(product_to_map(Product)), Req2),
                    {ok, Req3, undefined};
                {error, Reason} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                            jiffy:encode(#{error => Reason}), Req2),
                    {ok, Req3, undefined}
            end
    end.

handle_get(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    case product_store:get(Id) of
        {ok, Product} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">>=><<"application/json">>}, jiffy:encode(product_to_map(Product)), Req),
            {ok, Req2, undefined};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>},
                                   jiffy:encode(#{error => <<"not_found">>}), Req),
            {ok, Req2, undefined}
    end.

handle_update(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    {ok, BodyBin, Req2} = cowboy_req:read_body(Req),
    case catch jiffy:decode(BodyBin, [return_maps]) of
        {'EXIT', _} ->
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                    jiffy:encode(#{error => <<"invalid_json">>}), Req2),
            {ok, Req3, undefined};
        Map ->
            case product_store:update(Id, Map) of
                {ok, Product} ->
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(product_to_map(Product)), Req2),
                    {ok, Req3, undefined};
                {error, not_found} ->
                    Req3 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => <<"not_found">>}), Req2),
                    {ok, Req3, undefined};
                {error, Reason} ->
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(#{error => Reason}), Req2),
                    {ok, Req3, undefined}
            end
    end.

handle_delete(Req, IdBin) ->
    Id = utils:to_int(binary_to_list(IdBin)),
    ok = product_store:delete(Id),
    Req2 = cowboy_req:reply(204, #{}, <<>> , Req),
    {ok, Req2, undefined}.
