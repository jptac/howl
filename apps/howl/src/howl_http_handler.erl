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

get_token(State, Req) ->
    case wiggle_h:get_ws_token(Req) of
        {ok, Token, SPerms, Req1} ->
            State1 = State#wsstate{token = Token, scope_perms = SPerms},
            {ok, Req1, State1};
        {Error, Req1} ->
            {Error, Req1, State}
    end.

%% Fuck you dialyzer, it refuses to accept that get_token can
%% return stuff other then no_token ...
-dialyzer({[no_match], [get_token/2, websocket_init/3]}).
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
        {no_token, Req3, _State1} ->
            {ok, Req4} = cowboy_req:reply(401, [], <<"ott required">>,
                                          Req3),
            {shutdown, Req4}
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
