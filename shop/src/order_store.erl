-module(order_store).
-behaviour(gen_server).

%% API
-export([start_link/0, create/1, get/1, all/0, update/2, delete/1]).

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

%%% API %%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Map) when is_map(Map) ->
    gen_server:call(?MODULE, {create, Map}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

all() ->
    gen_server:call(?MODULE, all).

update(Id, Map) when is_map(Map) ->
    gen_server:call(?MODULE, {update, Id, Map}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

%%% gen_server callbacks %%%
init([]) ->
    File = atom_to_list(?TABLE) ++ ".dets",
    case dets:open_file(?TABLE, [{file, File}]) of
        {ok, Tid} -> {ok, Tid};
        {error, Reason} ->
            io:format("Failed to open dets ~p: ~p~n", [?TABLE, Reason]),
            {stop, Reason}
    end.

handle_call({create, Map}, _From, Tid) ->
    % Validate FK: check product exists
    FK = case maps:get(<<"fk_product_id">>, Map, maps:get(fk_product_id, Map, undefined)) of
             N when is_integer(N) -> N;
             Bin when is_binary(Bin) ->
                case catch list_to_integer(binary_to_list(Bin)) of
                    I when is_integer(I) -> I;
                    _ -> undefined
                end;
             _ -> undefined
         end,
    case FK of
        undefined ->
            {reply, {error, invalid_fk}, Tid};
        _ ->
            % check via product_store
            case product_store:get(FK) of
                {ok, _Prod} ->
                    Id = erlang:unique_integer([monotonic, positive]),
                    Created = utils:now_iso(),
                    Order = #order{
                        id = Id,
                        username = maps:get(<<"username">>, Map, maps:get(username, Map, <<"">>)),
                        fk_product_id = FK,
                        created_at = Created,
                        order_type = maps:get(<<"order_type">>, Map, maps:get(order_type, Map, <<"">>))
                    },
                    dets:insert(Tid, {Id, Order}),
                    {reply, {ok, Order}, Tid};
                {error, not_found} ->
                    {reply, {error, fk_not_found}, Tid}
            end
    end;

handle_call({get, Id}, _From, Tid) ->
    case dets:lookup(Tid, Id) of
        [{_, Order}] -> {reply, {ok, Order}, Tid};
        [] -> {reply, {error, not_found}, Tid}
    end;

handle_call(all, _From, Tid) ->
    FoldFun = fun ({_K, P}, Acc) -> [P|Acc] end,
    List = dets:foldl(FoldFun, [], Tid),
    {reply, {ok, lists:reverse(List)}, Tid};

handle_call({update, Id, Map}, _From, Tid) ->
    case dets:lookup(Tid, Id) of
        [{_, Order}] ->
            % allow updating username and order_type and fk (with FK validation)
            NewFK = case maps:get(<<"fk_product_id">>, Map, Order#order.fk_product_id) of
                        N when is_integer(N) -> N;
                        Bin when is_binary(Bin) ->
                            case catch list_to_integer(binary_to_list(Bin)) of
                                I when is_integer(I) -> I;
                                _ -> Order#order.fk_product_id
                            end;
                        _ -> Order#order.fk_product_id
                    end,
            case product_store:get(NewFK) of
                {ok, _Prod} ->
                    New = Order#order{
                        username = maps:get(<<"username">>, Map, Order#order.username),
                        fk_product_id = NewFK,
                        order_type = maps:get(<<"order_type">>, Map, Order#order.order_type)
                    },
                    dets:insert(Tid, {Id, New}),
                    {reply, {ok, New}, Tid};
                {error, not_found} ->
                    {reply, {error, fk_not_found}, Tid}
            end;
        [] -> {reply, {error, not_found}, Tid}
    end;

handle_call({delete, Id}, _From, Tid) ->
    dets:delete(Tid, Id),
    {reply, ok, Tid};

handle_call(_Req, _From, Tid) ->
    {reply, {error, unknown}, Tid}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, Tid) ->
    catch dets:close(Tid),
    ok.
code_change(_OldV, State, _Extra) -> {ok, State}.
