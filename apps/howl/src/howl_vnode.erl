-module(howl_vnode).
-behaviour(riak_core_vnode).

-include("howl.hrl").

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         repair/4,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([leave/4, listen/4, listeners/3]).


-ignore_xref([
              start_vnode/1,
              listen/4,
              leave/4,
              repair/4,
              listeners/3
             ]).

-record(state, {partition,
                node,
                channels,
                listeners}).

-define(MASTER, howl_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition,
                  node = node(),
                  channels = [],
                  listeners = []}}.

repair(IdxNode, Channel, VClock, Obj) ->
    riak_core_vnode_master:command(IdxNode,
                                   {repair, Channel, VClock, Obj},
                                   ignore,
                                   ?MASTER).

listen(Preflist, ReqID, Channel, Listener) ->
    riak_core_vnode_master:command(Preflist,
                                   {listen, ReqID, Channel, Listener},
                                   {fsm, undefined, self()},
                                   ?MASTER).

leave(Preflist, ReqID, Channel, Listener) ->
    riak_core_vnode_master:command(Preflist,
                                   {leave, ReqID, Channel, Listener},
                                   {fsm, undefined, self()},
                                   ?MASTER).

listeners(Preflist, ReqID, Channel) ->
    riak_core_vnode_master:command(Preflist,
                                   {listeners, ReqID, Channel},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({repair, Channel, _VClock, #howl_obj{val=Val0} = Obj}, _Sender,
               #state{channels=Channels0}=State) ->
    Listeners = [{L, Channel} || L <- statebox:value(Val0)],
    Channels1 = lists:keystore(Channel, 1, Channels0, {Channel, Obj}),
    {noreply, State#state{channels = Channels1,
                          listeners = Listeners ++ State#state.listeners}};

handle_command({listeners, ReqID, Channel}, _Sender, #state{channels=Channels0, partition=Partition, node=Node} = State) ->
    Res = case lists:keyfind(Channel, 1, Channels0) of
              false ->
                  {ok, ReqID, {Partition, Node}, not_found};
              {Channel, V} ->
                  {ok, ReqID, {Partition,Node}, V}
          end,
    {reply, Res, State};

handle_command({listen, {ReqID, Coordinator}, Channel, Listener}, _Sender,
               #state{channels = Channels0,
                      listeners = Listeners0} = State) ->
    case lists:keyfind(Channel, 1, Channels0) of
        false ->
            Val0 = statebox:new(fun howl_entity_state:new/0),
            Val1 = statebox:modify({fun howl_entity_state:add/2, [Listener]}, Val0),
            VC0 = vclock:fresh(),
            VC = vclock:increment(Coordinator, VC0),
            Obj = #howl_obj{val=Val1, vclock=VC},
            Channels1 = [{Channel, Obj}|Channels0],
            riak_core_vnode:monitor({raw, erlang:make_ref(), Listener}),
            {reply, {ok, ReqID}, State#state{channels=Channels1,
                                             listeners=[{Listener, Channel}|Listeners0]}};
        {Channel, #howl_obj{val=Val0} = O} ->
            Val1 = statebox:modify({fun howl_entity_state:add/2, [Listener]}, Val0),
            Val2 = statebox:expire(?STATEBOX_EXPIRE, Val1),
            Obj = howl_obj:update(Val2, Coordinator, O),
            Channels1 = lists:keystore(Channel, 1, Channels0, {Channel, Obj}),
            riak_core_vnode:monitor({raw, erlang:make_ref(), Listener}),
            {reply, {ok, ReqID}, State#state{channels=Channels1,
                                             listeners=[{Listener, Channel}|Listeners0]}}
    end;

handle_command({leave, {ReqID, Coordinator}, Channel, Listener}, _Sender,
               #state{channels = Channels0,
                      listeners = Listeners0} = State) ->
    Listeners1 = lists:delete({Listener, Channel}, Listeners0),
    Channels1 = case lists:keyfind(Channel, 1, Channels0) of
                    {Channel, #howl_obj{val=Val0} = O} ->
                        Val1 = statebox:modify({fun howl_entity_state:remove/2, [Listener]}, Val0),
                        Val2 = statebox:expire(?STATEBOX_EXPIRE, Val1),
                        case statebox:value(Val2) of
                            [] ->
                                lists:keydelete(Channel, 1, Channels0);
                            _ ->
                                Obj = howl_obj:update(Val2, Coordinator, O),
                                lists:keystore(Channel, 1, Channels0, {Channel, Obj})
                        end;
                    _ ->
                        Channels0
                end,
    {reply, {ok, ReqID}, State#state{channels=Channels1,
                                     listeners=Listeners1}};


handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = lists:foldl(fun ({C, Ls}, AccIn) ->
                              Fun(C, Ls, AccIn)
                      end, Acc0, State#state.channels),
    {reply, Acc, State};

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, State) ->
    {Channel, #howl_obj{val=Val0} = O} = binary_to_term(Data),
    Listeners = [{L, Channel} || L <- statebox:value(Val0)],
    Channels = lists:keystore(Channel, 1, State#state.channels, {Channel, O}),
    {reply, ok, State#state{channels = Channels,
                            listeners = Listeners ++ State#state.listeners}}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State) ->
    case State#state.channels of
        [] ->
            {true, State};
        _ ->
            {false, State}
    end.

delete(State) ->
    {ok, {ok, State#state{channels=[],
                          listeners = []}}}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(Listener, _Reason, State) ->
    {noreply,delete_listener(Listener, State)}.

terminate(_Reason, _State) ->
    ok.

delete_listener(Listener, State = #state{channels=Channels0, listeners=Listeners}) ->
    {ToDelete, Listeners1} = lists:partition(fun ({AListener, _}) ->
                                                     case AListener of
                                                         Listener ->
                                                             true;
                                                         _ ->
                                                             false
                                                     end
                                             end, Listeners),
    Channels2 = lists:foldl(fun ({_, Channel}, Channels1) ->
                                    case lists:keyfind(Channel, 1, Channels1) of
                                        {Channel, #howl_obj{val=Val0} = O} ->
                                            Val1 = statebox:modify({fun howl_entity_state:remove/2, [Listener]}, Val0),
                                            Val2 = statebox:expire(?STATEBOX_EXPIRE, Val1),
                                            case statebox:value(Val2) of
                                                [] ->
                                                    lists:keydelete(Channel, 1, Channels1);
                                                _ ->
                                                    Obj = howl_obj:update(Val2, Listener, O),
                                                    lists:keystore(Channel, 1, Channels1, {Channel, Obj})
                                            end;
                                        _ ->
                                            Channels1
                                    end
                            end, Channels0, ToDelete),
    State#state{listeners = Listeners1,
                channels = Channels2}.
