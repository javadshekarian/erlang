-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, handle_client_msg/2, websocket_info/2, terminate/3]).

%%% init: called first by cowboy:
init(Req, _Otp) -> 
    {cowboy_websocket, Req, #{username => undefined}}.


%%% websocket_init: called after handshake:
websocket_init(State) ->
    {ok, State}.

%%% websocket_handle: receive message from client:
websocket_handle({text, MsgBin}, State) ->
    case catch jsx:decode(MsgBin, [return_maps]) of
        {'EXIT', _} ->
            Reply = jsx:encode(#{error => <<"invalid_json">>}),
            {reply, {text, Reply}, State};
        MsgMap ->
            handle_client_msg(MsgMap, State)
    end;

websocket_handle(_, State) ->
    {ok, State}.

handle_client_msg(MsgMap, State) ->
    Type = maps:get(<<"type">>, MsgMap, undefined),

    case Type of
        <<"register">> ->
            Username = maps:get(<<"username">>, MsgMap, undefined),
            case Username of
                undefined ->
                    Reply = jsx:encode(#{error => <<"no_username">>}),
                    {reply, {text, Reply}, State};
                _ ->
                    ets:insert(users, {Username, self()}),
                    NewState = State#{username => Username},
                    Reply = jsx:encode(#{status => <<"ok">>, type => <<"registered">>}),
                    {reply, {text, Reply}, NewState}
            end;

        <<"message">> ->
            To   = maps:get(<<"to">>, MsgMap, undefined),
            Body = maps:get(<<"body">>, MsgMap, <<>>),
            From = maps:get(username, State, undefined),
            io:format("To: ~p~n, Body: ~p~n, username: ~p~n", [To, Body, From]),
            
            case From of
                undefined ->
                    Reply = jsx:encode(#{error => <<"not_registered">>}),
                    {reply, {text, Reply}, State};
                _ ->
                    case ets:lookup(users, To) of
                        [] ->
                            Reply = jsx:encode(#{error => <<"user_not_online">>}),
                            {reply, {text, Reply}, State};
                        [{_, Pid}] ->
                            %% ارسال پیام داخلی
                            Pid ! {ws_msg, From, Body},
                            Ack = jsx:encode(#{status => <<"sent">>}),
                            {reply, {text, Ack}, State}
                    end
            end;

        _ ->
            Reply = jsx:encode(#{error => <<"unknown_type">>}),
            {reply, {text, Reply}, State}
    end.

websocket_info({ws_msg, From, Body}, State) ->
    Out = jsx:encode(#{
        type => <<"message">>,
        from => From,
        body => Body
    }),
    {reply, {text, Out}, State};

websocket_info(_, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    case maps:get(<<"username">>, State, undefined) of
        undefined -> ok;
        Username  -> ets:delete(users, Username)
    end,
    ok.