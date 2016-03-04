%% @doc Interface for howl-admin commands.
-module(howl_console).
-export([connections/1]).
-ignore_xref([connections/1]).


connections(["snarl"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsnarl:servers());

connections(["sniffle"]) ->
    io:format("Sniffle endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections([]) ->
    case {connections(["snarl"]),
          connections(["sniffle"])} of
        {ok, ok} ->
            ok;
        _ ->
            error
    end.

print_endpoints(Es) ->
    io:format("Hostname            "
              "                    "
              " Node               "
              " Errors    ~n"),
    io:format("--------------------"
              "----------"
              " --------------------"
              " ---------------~n", []),
    [print_endpoint(E) || E <- Es],
    case [print_endpoint(E) || E <- Es] of
        [] ->
            error;
        _ ->
            ok
    end.


print_endpoint({{Hostname, [{port, Port}, {ip, IP}]}, _, Fails}) ->
    HostPort = <<IP/binary, ":", Port/binary>>,
    io:format("~30s ~-24s ~9b~n", [Hostname, HostPort, Fails]).
