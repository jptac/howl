%%-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(PRINT(Var), 1 == 1 orelse io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(N, 3).
-define(R, 2).
-define(W, 3).
-define(STATEBOX_EXPIRE, 60000).
-define(DEFAULT_TIMEOUT, 10000).


-type val() ::  statebox:statebox().

-record(howl_obj, {val    :: val(),
		    vclock :: vclock:vclock()}).

-type howl_obj() :: #howl_obj{} | not_found.

-type idx_node() :: {integer(), node()}.

-type vnode_reply() :: {idx_node(), howl_obj() | not_found}.
