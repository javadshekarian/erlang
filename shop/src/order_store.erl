-module(order_store).
-behaviour(gen_server).

%% API
-export([start_link/0, create/1, get/1, all/0, update/2, stop/0, delete/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(order, {
    id,
    username,
    fk_product_id,
    created_at,
    order_type
}).

-define(TABLE, orders).
-define(SERVER, ?MODULE).

start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

create(Order) -> 
    gen_server:call(?SERVER, {create, Order}).

get(Id) ->
    gen_server:call(?SERVER, {get, Id}).

all() ->
    gen_server:call(?SERVER, all).

update(Id, OrderPartial) ->
    gen_server:call(?SERVER, {update, Id, OrderPartial}).

delete(Id) ->
    gen_server:call(?SERVER, {delete, Id}).

stop() -> 
    gen_server:stop(?SERVER).

init([]) ->
    FileName = atom_to_list(?TABLE) ++ ".dets",
    case dets:open_file(?TABLE, [{file, FileName}]) of
        {ok, Tid} -> {ok, Tid};
        {error, Reason} ->
            io:format("Failed to open dets ~p: ~p~n", [?TABLE, Reason]),
            {stop, Reason}
    end.

handle_call({create, OrderIndex}, _From, Tid) ->
    Created = utils:now_iso(),
    Id = erlang:unique_integer([monotonic, positive]),
    Order = #order{
        created_at = Created, 
        id = Id, 
        order_type = maps:get(<<"order_type">>, OrderIndex, undefined), 
        username = maps:get(<<"username">>, OrderIndex, undefined), 
        fk_product_id = maps:get(<<"fk_product_id">>, OrderIndex, undefined)
    },
    case dets:insert(Tid, {Id, Order}) of
        ok -> {reply, {ok, Order}, Tid};
        {error, Reason} -> {reply, {error, Reason}, Tid}
    end;

handle_call({update, Id, OrderPartial}, _From, Tid) ->
    LookupedOrder = dets:lookup(Tid, Id),
    case LookupedOrder of
        [] -> 
            {reply, {error, unknown}, Tid};
        [{_Key, Order}] ->
            UpdatedOrder = Order#order {
                created_at = Order#order.created_at,
                order_type = maps:get(<<"order_type">>, OrderPartial, Order#order.order_type),
                username = maps:get(<<"username">>, OrderPartial, Order#order.username),
                fk_product_id = maps:get(<<"fk_product_id">>, OrderPartial, Order#order.fk_product_id)
            },
            case dets:insert(Tid, {Id, UpdatedOrder}) of
                ok -> {reply, {ok, UpdatedOrder}, Tid};
                {error, Reason} -> {reply, {error, Reason}, Tid}
            end
    end;

handle_call({get, Id}, _From, Tid) ->
    case dets:lookup(Tid, Id) of
        [{_Key, Order}] -> {reply, {ok, Order}, Tid};
        [] -> {reply, {error, unknown}, Tid}
    end;

handle_call(all, _From, Tid) ->
    FoldFun = fun ({_K, P}, Acc) -> [P|Acc] end,
    List = dets:foldl(FoldFun, [], Tid),
    {reply, {ok, lists:reverse(List)}, Tid};

handle_call({delete, Id}, _From, Tid) ->
    case dets:delete(Tid, Id) of
        ok -> {reply, ok, Tid};
        {error, Reason} -> {reply, {error, Reason}, Tid}
    end;

handle_call(_Other, _From, Tid) ->
    {reply, ok, Tid}.


handle_cast(_Msg, Tid) -> {noreply, Tid}.

handle_info(_Info, Tid) -> {noreply, Tid}.

terminate(_Reason, Tid) -> 
    dets:close(Tid),
    ok.

code_change(_OldVersion, Tid, _Extra) -> {ok, Tid}.