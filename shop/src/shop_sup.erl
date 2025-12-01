-module(shop_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Dispatch = cowboy_router:compile([
        {'_',[
            {"/product", product_handler, []},
            {"/product/:id", product_handler, []},
            {"/order", order_handler, []},
            {"/order/:id", order_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8082}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    Children = [
        {product_store, {product_store, start_link, []}, permanent, 5000, worker, [product_store]},
        {order_store, {order_store, start_link, []}, permanent, 5000, worker, [order_store]}
    ],

    {ok, {{one_for_one, 5, 10}, Children}}.

%% internal functions
