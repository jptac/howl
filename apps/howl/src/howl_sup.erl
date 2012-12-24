-module(howl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { howl_vnode_master,
                  {riak_core_vnode_master, start_link, [howl_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    WriteFSMs = { howl_entity_write_fsm_sup,
		{ howl_entity_write_fsm_sup, start_link, []},
		permanent, infinity, supervisor, [howl_entity_write_fsm_sup]},

    CoverageFSMs = { howl_entity_coverage_fsm_sup,
		     { howl_entity_coverage_fsm_sup, start_link, []},
		    permanent, infinity, supervisor, [howl_entity_coverage_fsm_sup]},
    ReadFSMs = {
      howl_entity_read_fsm_sup,
      {
	howl_entity_read_fsm_sup, start_link, []},
      permanent, infinity, supervisor, [howl_entity_read_fsm_sup]},

    Dispatch = [{'_', [{'_', howl_http_handler, []}]}],
    {ok, HTTPPort} = application:get_env(http_port),
    {ok, Accpetors} = application:get_env(accpetors),
    cowboy:start_listener(howl_listener, Accpetors,
			  cowboy_tcp_transport, [{port, HTTPPort}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]
			 ),
    { ok,
        { {one_for_one, 5, 10},
          [VMaster, WriteFSMs, ReadFSMs, CoverageFSMs]}}.

