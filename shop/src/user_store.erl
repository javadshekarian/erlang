-module(user_store).
-behaviour(gen_server).

-export([start_link/0, create/1, get/1, all/0, update/2, delete/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, users).

-record(user, {
    id,
    username,
    firstname,
    lastname,
    age,
    email,
    created_at
}).

start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

create(User) -> 
    gen_server:call(?SERVER, {create, User}).

get(Id) ->
    gen_server:call(?SERVER, {gen, Id}).

all() -> 
    gen_server:call(?SERVER, all).

update(Id, PartialUser) ->
    gen_server:call(?SERVER, {update, Id, PartialUser}).

delete(Id) -> 
    gen_server:call(?SERVER, {delete, Id}).

stop() -> 
    gen_server:stop(?SERVER).

init([]) ->
    Filename = atom_to_list(?TABLE) ++ ".dets",
    case dets:open_file(?TABLE, [{file, Filename}]) of 
        {ok, Tid} -> {ok, Tid};
        {error, Reason} -> 
            io:format("Fail To Open ~p: ~p~n",[?TABLE, Reason]),
            {stop, Reason}
    end.

handle_call({create, User}, _From, Tid) -> 
    Created = utils:now_iso(),
    Id = erlang:unique_integer([monotonic, positive]),
    User = #user{
        age = maps:get(<<"age">>, User, undefined), 
        email = maps:get(<<"email">>, User, undefined), 
        firstname = maps:get(<<"firstname">>, User, undefined), 
        id = Id, 
        lastname = maps:get(<<"lastname">>, User, undefined), 
        username = maps:get(<<"username">>, User, undefined),
        created_at = Created
    },
    case dets:insert(Tid, {Id, User}) of
      ok -> {reply, {ok, User}, Tid};
      {error, Reason} -> {reply, {error, Reason}, Tid}
    end;

handle_call({update, Id, PartialUser}, _From, Tid) ->
    case dets:lookup(Tid, Id) of
        [] -> {error, unknown, Tid};
        [{_Key, User}] ->
            UpdatedUser = User#user{
                username = maps:get(<<"username">>, PartialUser, User#user.id),
                firstname = maps:get(<<"firstname">>, PartialUser, User#user.firstname),
                lastname = maps:get(<<"lastname">>, PartialUser, User#user.lastname),
                email = maps:get(<<"email">>, PartialUser, User#user.email),
                age = maps:get(<<"age">>, PartialUser, User#user.age)
            },
            case dets:insert(Tid, {Id, UpdatedUser}) of 
                ok -> {reply, {ok, UpdatedUser}, Tid};
                {error, Reason} -> {reply, {error, Reason}, Tid}
            end
        end;

handle_call(all, _From, Tid) ->
    FoldFun = fun({_K, P}, Acc) -> [P|Acc] end,
    List = dets:foldl(FoldFun, [], Tid),
    {reply, {ok, lists:reverse(List)}, Tid};

handle_call({get, Id}, _From, Tid) -> 
    case dets:lookup(Id, Tid) of 
        [{_Key, User}] -> {reply, {ok, User}, Tid};
        [] -> {reply, {error, unknown}, Tid}
    end;

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