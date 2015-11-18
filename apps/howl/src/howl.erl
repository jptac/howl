-module(howl).
-include("howl.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         listen/1,
         leave/1,
         listeners/1,
         send/2
        ]).

-ignore_xref([listeners/1]).

%% Public API

leave(Channel) ->
    howl_entity_write_fsm:write({howl_vnode, howl}, Channel, leave, self()).

listen(Channel) ->
    lager:info("[~s] join ~p.", [Channel, self()]),
    howl_entity_write_fsm:write({howl_vnode, howl}, Channel, listen, self()).

listeners(Channel) ->
    howl_entity_read_fsm:start({howl_vnode, howl}, listeners, Channel).

send(Channel, Message) ->
    case listeners(Channel) of
        {ok, not_found} ->
            ok;
        {ok, Listeners} ->
            M = {msg, [{<<"channel">>, Channel}, {<<"message">>, Message}]},
            [L ! M || L <- Listeners],
            ok
    end,
    ok.

-ifdef(TEST).

dummy_test() ->
    ?assertEqual(1, 1).

-endif.
