-module(howl_tcp_protocol).

-export([init/1, message/2]).

-ignore_xref([init/1, message/2]).

-include("howl_version.hrl").

init([]) ->
    {ok, stateless}.

%%%===================================================================
%%%  VM Functions
%%%===================================================================

message({msg, Channel, Msg}, State) ->
    howl:send(Channel, Msg),
    {noreply, State};

message(version, State) ->
    {reply, ?VERSION, State};

message(Oops, State) ->
    io:format("oops: ~p~n", [Oops]),
    {stop, State}.
