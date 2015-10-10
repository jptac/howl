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
    {noreply, State};

message({msg, Msgs}, State) ->
    [howl:send(Channel, Msg) || {Channel, Msg} <- Msgs],
    {noreply, State};

message(version, State) ->
    {reply, {ok, ?VERSION}, State};

message(_Oops, State) ->
    {stop, State}.
