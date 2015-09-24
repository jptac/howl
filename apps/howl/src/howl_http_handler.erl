-module(howl_http_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).

-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
              websocket_info/3, websocket_terminate/3]).
-ignore_xref([init/3, handle/2, terminate/2]).

-record(wsstate, {token :: undefined | binary() | {token, binary()},
                  encoder :: fun((term()) -> binary()),
                  decoder :: fun((binary()) -> term()),
                  type = text :: text | binary,
                  channels=[],
                  scope_perms = [[<<"...">>]]}).

-type state() :: #wsstate{}.

init({_Andy, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


init_state(C = <<"msgpack">>, Req) ->
    Req1 = cowboy_req:set_resp_header(
             <<"sec-websocket-protocol">>, C,  Req),
    {#wsstate{
        encoder =fun(O) ->
                         msgpack:pack(O, [jsx])
                 end,
        decoder = fun(D) ->
                          case msgpack:unpack(D, [jsx]) of
                              {ok, O} ->
                                  jsxd:from_list(O);
                              _ ->
                                  []
                          end
                  end,
        type = binary},
     Req1};
init_state(C = <<"json">>, Req) ->
    Req1 = cowboy_req:set_resp_header(
             <<"sec-websocket-protocol">>, C,  Req),
    {#wsstate{
        encoder =
            fun(O) ->
                    jsx:encode(O)
            end,
        decoder =
            fun(D) ->
                    jsxd:from_list(jsx:decode(D))
            end},
     Req1};
init_state(unset, Req) ->
    {#wsstate{
        encoder =
            fun(O) ->
                    jsx:encode(O)
            end,
        decoder =
            fun(D) ->
                    jsxd:from_list(jsx:decode(D))
            end},
     Req}.
-spec get_ott(Req) ->
                     {binary(), Req} |
                     {undefined, Req}
                         when Req :: cowboy_req:req().
get_ott(Req) ->
    cowboy_req:qs_val(<<"fifo_ott">>, Req).

%% Fuck you dialyzer, it refuses to accept that get_token can
%% return stuff other then no_token ...
-dialyzer({nowarn_function, [get_token/2]}).
-spec get_token(State, Req) ->
                       {ok, Req, State} |
                       {denied, Req, State} |
                       {no_token, Req, State}
                           when Req :: cowboy_req:req(),
                                State :: state().
get_token(State, Req) ->
    case get_ott(Req) of
        {OTT, Req1} when is_binary(OTT) ->
            case ls_token:get(OTT) of
                {ok, Bearer} ->
                    ls_token:delete(OTT),
                    case ls_oauth:verify_access_token(Bearer) of
                        {ok, Context} ->
                            case {proplists:get_value(<<"resource_owner">>, Context),
                                  proplists:get_value(<<"scope">>, Context)} of
                                {undefined, _} ->
                                    {denied, Req1, State};
                                {UUID, Scope} ->
                                    Scopes = ls_oauth:scope(Scope),
                                    SPerms = cowboy_oauth:scope_perms(Scopes, []),
                                    State1 = State#wsstate{token = UUID, scope_perms = SPerms},
                                    {ok, Req1, State1}
                            end;
                        _ ->
                            {denied, Req1, State}
                    end;
                _ ->
                    {denied, Req1, State}
            end;
        {undefined, Req1} ->
            {no_token, Req1, State}
    end.

%% Fuck you dialyzer, it refuses to accept that get_token can
%% return stuff other then no_token ...
-dialyzer({[no_match], [websocket_init/3]}).
-spec websocket_init(term(), cowboy_req:req(), []) ->
                            {ok, cowboy_req:req(), state()} |
                            {shutdown, cowboy_req:req()}.
websocket_init(_Any, Req, []) ->
    {ok, [C|_], Req1} =
        cowboy_req:parse_header(
          <<"sec-websocket-protocol">>,
          Req, [unset]),
    {State, Req2} = init_state(C, Req1),
    case get_token(State, Req2) of
        {ok, Req3, State1} ->
            {ok, Req3, State1};
        {denied, Req3, _State1} ->
            {ok, Req4} = cowboy_req:reply(403, [], <<"permission denied">>,
                                          Req3),
            {shutdown, Req4};
        {no_token, Req3, State1} ->
            case cowboy_req:binding(version, Req3) of
                {<<"0.1.0">>, Req4} ->
                    {ok, Req4, State1};
                {_, Req4} ->
                    {ok, Req5} = cowboy_req:reply(401, [], <<"ott required">>,
                                                  Req4),
                    {shutdown, Req5}
            end
    end.


websocket_handle({Type, Raw}, Req,
                 State = #wsstate{type = Type, decoder = Dec, encoder = Enc}) ->
    case handle_data(Dec(Raw), State) of
        {reply, Reply, State1} ->
            {reply, {Type, Enc(Reply)}, Req, State1};
        {ok, State1} ->
            {ok, Req, State1}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State = #wsstate{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(normal, _Req, #wsstate{channels = Cs}) ->
    [howl:leave(C) || C <- Cs],
    ok;

websocket_terminate(Reason, _Req, State) ->
    lager:warning("terminating websocket with reason ~p when state was ~p.",
                  [Reason, State]),
    ok.

handle_data([{<<"ping">>, V}], State) ->
    {reply, [{<<"pong">>, V}], State};

handle_data([{<<"token">>, Token}], State) ->
    State1 = State#wsstate{token = {token, Token}},
    {reply, [{<<"ok">>, <<"authenticated">>}], State1};

handle_data([{<<"bearer">>, Bearer}], State) ->
    case ls_oauth:verify_access_token(Bearer) of
        {ok, Context} ->
            case {proplists:get_value(<<"resource_owner">>, Context),
                  proplists:get_value(<<"scope">>, Context)} of
                {undefined, _} ->
                    {reply, [{<<"error">>, <<"access_denied">>}], State};
                {UUID, Scope} ->
                    SPerms = cowboy_oauth:scope_perms(ls_oauth:scope(Scope), []),
                    State1 = State#wsstate{token = UUID, scope_perms = SPerms},
                    {reply, [{<<"ok">>, <<"authenticated">>}], State1}
            end;
        _ ->
            {reply, [{<<"error">>, <<"access_denied">>}], State}
    end;

handle_data(_, State = #wsstate{token = undefined}) ->
    {reply, [{<<"error">>, <<"not authenticated">>}], State};

handle_data([{<<"join">>, Channel}],
            State = #wsstate{token = Token, channels=Cs, scope_perms = SP}) ->
    Permission = [<<"channels">>, Channel, <<"join">>],
    case libsnarlmatch:test_perms(Permission, SP) andalso
        libsnarl:allowed(Token, Permission) of
        true ->
            howl:listen(Channel),
            State1 = State#wsstate{channels=[Channel | Cs]},
            {reply, [{<<"ok">>, <<"channel joined">>}], State1};
        _ ->
            {reply, [{<<"error">>, <<"permission denied">>}], State}
    end;


handle_data([{<<"leave">>, Channel}],
            State = #wsstate{channels=Cs}) ->
    howl:leave(Channel),
    State1 = State#wsstate{channels=[C || C <- Cs, C =/= Channel]},
    {reply, [{<<"ok">>, <<"channel left">>}], State1};

handle_data(_JSON, State) ->
    {ok, State}.
