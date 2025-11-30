-module(educate).
-export([hello/0, run/0, send/0, worker/0, sender/0, receiver_1/0, loop/0, sendArg/1, receiveArg/1]).

hello() -> 
    io:format("Hello From Erlang! ~n").

% create process
run() ->
    spawn(fun() -> 
        io:format("Hello From Process! ~n")
    end).

% send and receive message between process 
% send message
send() -> 
    Pid = spawn(educate, worker, []),
    Pid ! {hi, "Erlang"},
    Pid ! stop,
    io:format("Pid Value: ~p ~p ~p ~p ~n",[Pid,"javad","shekarian","25"]),
    ok.

worker() ->
    receive
      Msg ->
        io:format("Worker Get Mesage From ~p!~n", [Msg])
    end.

% test several receive switch and test loop:
sender() -> 
    Pid = spawn(educate, receiver_1, []),
    Pid ! {javad, shekarian, "some message"},
    Pid ! stop,
    ok.

receiver_1() -> 
    receive 
        {javad, shekarian, Msg} -> 
            io:format("Message Is Received, Channel Name: ~p ~p, Message Content: ~p. ~n", [javad, shekarian, Msg]), 
            receiver_1();
        stop ->
            io:format("Channel Name: ~p, Stop Receiving And Calling Function ... ~n", [stop])
    end.


% write loop: in erlang we not have while and for 
% and every loop will be written with CTE
loop() -> 
    io:format("Hello! This Is A Loop! ~n"),
    loop().

% send arguman to process in a functin 
% and receive it in another function 
sendArg(Name) -> 
    spawn(educate, receiveArg, [Name]).

receiveArg(Name) ->
    case Name of
        {javad, shekarian, Msg} -> 
            io:format("Received Arguman From sendArg: ~p. ~n", [Name]),
            receiveArg({javad, shekarian, Msg});
        stop ->
            io:format("Stop Is Runned! Stoping ...~n");
        _ ->
            io:format("Unknown Message")
    end.