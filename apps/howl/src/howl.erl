-module(howl).
-include("howl.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
	 listen/1,
	 send/2
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, howl),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, howl_vnode_master).


listen(Channel) ->
    howl_entity_write_fsm:write(
      { 
       howl_vnode, 
       howl
      }, Channel, listen, self()).


send(Channel, Message) ->
    case howl_entity_read_fsm:start({howl_vnode, howl}, listeners, Channel) of
	{ok, not_found} ->
	    ok;
	{ok, Listeners} ->
	    M = {msg, Message},
	    [L ! M || L <- Listeners],
	    ok
	end.
