-module(jp_extras_h).
-include_lib("wiggle/include/wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CACHE, user).
-define(LIST_CACHE, user_list).
-define(FULL_CACHE, user_full_list).

-export([allowed_methods/3,
         permission_required/2,
         create/3,
         delete/2,
         get/1,
         read/2,
         write/3,
         to_json/1]).

-behaviour(wiggle_rest_h).

allowed_methods(_V, _Token, [?UUID(_User), <<"tokens">>]) ->
    [<<"POST">>].

permission_required(post, [?UUID(User), <<"tokens">>]) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(_Method, _Path) ->
    undefined.

%%-------------------------------------------------------------------
%% Stubs for behavior type
%%-------------------------------------------------------------------

delete(Req, State) ->
    {halt, Req, State}.
get(_) ->
    not_found.
read(Req, State) ->
    {halt, Req, State}.
write(Req, State, _) ->
    {halt, Req, State}.

%%--------------------------------------------------------------------
%% POST
%%--------------------------------------------------------------------

create(Req, State = #state{path = [?UUID(User), <<"tokens">>]},
       [{<<"comment">>, Comment}, {<<"scope">>, Scope}, {<<"token">>, Token}]) ->
    case ls_user:manual_token(User, Scope, Comment, Token) of
        {error, no_servers}->
            {ok, Req1} = cowboy_req:reply(404, [], <<"Server not found">>, Req),
            {halt, Req1, State};
        not_found ->
            {ok, Req1} = cowboy_req:reply(404, [], <<"User not found">>, Req),
            {halt, Req1, State};
        {error, bad_scope} ->
            {ok, Req1} = cowboy_req:reply(404, [], <<"Bad scope">>, Req),
            {halt, Req1, State};
        {ok, {TokenID, Token}} ->
            e2qc:evict(?CACHE, User),
            e2qc:teardown(?FULL_CACHE),
            {MediaType, Req1} =
                case cowboy_req:meta(media_type, Req) of
                    {{<<"application">>, <<"x-msgpack">>, _}, ReqX} ->
                        {msgpack, ReqX};
                    {{<<"application">>, <<"json">>, _}, ReqX} ->
                        {json, ReqX}
                end,
            J = [{<<"token">>, Token}, {<<"token-id">>, TokenID}],
            {Body, Req2} = wiggle_h:encode(J, MediaType, Req1),
            {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
            {halt, Req3, State}
    end;

create(Req, State = #state{path = Path}, X) ->
  {halt, Req, State}.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

to_json(U) ->
    U1 = ft_user:to_json(U),
    U2 = jsxd:delete([<<"password">>], U1),
    jsxd:update([<<"metadata">>],
                fun(M) ->
                        jsxd:get([<<"public">>], [{}], M)
                end, [{}], U2).
