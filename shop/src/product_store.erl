-module(product_store).
-behaviour(gen_server).

%% API
-export([start_link/0, create/1, get/1, all/0, update/2, delete/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(product, {
    id,
    productName,
    created_at,
    explain,
    mark
}).

-define(TABLE, products).

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
    % open dets table (file will be ./products.dets)
    File = atom_to_list(?TABLE) ++ ".dets",
    case dets:open_file(?TABLE, [{file, File}]) of
        {ok, Tid} ->
            {ok, Tid};
        {error, Reason} ->
            io:format("Failed to open dets ~p: ~p~n", [?TABLE, Reason]),
            {stop, Reason}
    end.

handle_call({create, Map}, _From, Tid) ->
    Id = erlang:unique_integer([monotonic, positive]),
    Created = utils:now_iso(),
    Product = #product{
        id = Id,
        productName = maps:get(<<"productName">>, Map, maps:get(productName, Map, <<"">>)),
        created_at = Created,
        explain = maps:get(<<"explain">>, Map, maps:get(explain, Map, <<"">>)),
        mark = maps:get(<<"mark">>, Map, maps:get(mark, Map, <<"">>))
    },
    case dets:insert(Tid, {Id, Product}) of
        ok -> {reply, {ok, Product}, Tid};
        {error, Reason} -> {reply, {error, Reason}, Tid}
    end;

handle_call({get, Id}, _From, Tid) ->
    case dets:lookup(Tid, Id) of
        [{_Key, Product}] -> {reply, {ok, Product}, Tid};
        [] -> {reply, {error, not_found}, Tid}
    end;

handle_call(all, _From, Tid) ->
    FoldFun = fun ({_K, P}, Acc) -> [P|Acc] end,
    List = dets:foldl(FoldFun, [], Tid),
    {reply, {ok, lists:reverse(List)}, Tid};

handle_call({update, Id, Map}, _From, Tid) -> 
    LookupResult = dets:lookup(Tid, Id),
    case LookupResult of
        [] ->
            {reply, {error, unknown}, Tid};
        [{_Key, Product}] ->
            UpdatedProduct = Product#product{
                explain = maps:get(<<"explain">>, Map, Product#product.explain),
                mark = maps:get(<<"mark">>, Map, Product#product.mark),
                productName = maps:get(<<"productName">>, Map, Product#product.productName),
                created_at = Product#product.created_at
            },
            case dets:insert(Tid, {Id, UpdatedProduct}) of
                ok ->
                    {reply, {ok, UpdatedProduct}, Tid};
                {error, Reason} ->
                    {reply, {error, Reason}, Tid}
            end
    end;

handle_call({delete, Id}, _From, Tid) ->
    case dets:delete(Tid, Id) of
        ok -> {reply, ok, Tid};
        {error, Reason} -> {reply, {error, Reason}, Tid}
    end;

handle_call(_Req, _From, Tid) ->
    {reply, {error, unknown}, Tid}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, Tid) ->
    catch dets:close(Tid),
    ok.
code_change(_OldV, State, _Extra) -> {ok, State}.
