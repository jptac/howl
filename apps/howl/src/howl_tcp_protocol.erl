-module(howl_tcp_protocol).

-export([init/2, message/2]).

-ignore_xref([init/2, message/2]).

-include("howl_version.hrl").

-record(state, {port}).

init(Prot, []) ->
    {ok, #state{port = Prot}}.

%%%===================================================================
%%%  VM Functions
%%%===================================================================

message({msg, Channel, Msg}, State) ->
    howl:send(Channel, Msg),
    {stop, normal, State};

message(version, State) ->
    {stop, normal, ?VERSION, State};

message(Oops, State) ->
    io:format("oops: ~p~n", [Oops]),
    {stop, State}.
