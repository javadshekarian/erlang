-module(utils).
-export([now_iso/0, to_int/1, ensure_binary/1]).

now_iso() ->
    {Date, Time} =
        calendar:system_time_to_universal_time(erlang:system_time(second), second),
    {{Y, M, D}, {HH, MM, SS}} = {Date, Time},
    lists:flatten(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Y, M, D, HH, MM, SS]
        )
    ).

to_int(Str) when is_list(Str) ->
    case catch list_to_integer(Str) of
        N when is_integer(N) -> N;
        _ -> undefined
    end;
to_int(N) when is_integer(N) -> N;
to_int(_) -> undefined.

ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(List) when is_list(List) -> list_to_binary(List);
ensure_binary(Other) -> list_to_binary(io_lib:format("~p", [Other])).