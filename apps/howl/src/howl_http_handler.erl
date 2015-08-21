-module(howl_http_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).

-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
              websocket_info/3, websocket_terminate/3]).
-ignore_xref([init/3, handle/2, terminate/2]).

-record(wsstate, {token, encoder, decoder, type, channels=[], 
                scope_perms = [[<<"...">>]]}).

init({_Andy, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, []) ->
    {ok, [C|_], Req0} =
        cowboy_req:parse_header(
          <<"sec-websocket-protocol">>,
          Req, [unset]),
    Req1 = cowboy_req:compact(Req0),
    {Encoder, Decoder, Type, Req2} =
        case C of
            <<"msgpack">> ->
                ReqX = cowboy_req:set_resp_header(
                         <<"sec-websocket-protocol">>, C,  Req1),
                {fun(O) ->
                         msgpack:pack(O, [jsx])
                 end,
                 fun(D) ->
                         case msgpack:unpack(D, [jsx]) of
                             {ok, O} ->
                                 jsxd:from_list(O);
                             _ ->
                                 []
                         end
                 end,
                 binary, ReqX};
            <<"json">> ->
                ReqX = cowboy_req:set_resp_header(
                         <<"sec-websocket-protocol">>, C,  Req1),
                {fun(O) ->
                         jsx:encode(O)
                 end,
                 fun(D) ->
                         jsxd:from_list(jsx:decode(D))
                 end, text, ReqX};
            unset ->
                {fun(O) ->
                         jsx:encode(O)
                 end,
                 fun(D) ->
                         jsxd:from_list(jsx:decode(D))
                 end, text, Req0}

        end,
    State = #wsstate{encoder = Encoder, decoder = Decoder, type = Type},

    R = case cowboy_req:qs_val(<<"fifo_ott">>, Req2) of
            {undefined, Req3} ->
                {no_token, Req3, State};
            {OTT, Req3} ->
                case ls_token:get(OTT) of
                    {ok, Bearer} ->
                        ls_token:delete(OTT),
                        case ls_oauth:verify_access_token(Bearer) of
                            {ok, Context} ->
                                case {proplists:get_value(<<"resource_owner">>, Context),
                                      proplists:get_value(<<"scope">>, Context)} of
                                    {undefined, _} ->
                                        {denied, Req3, State};
                                    {UUID, Scope} ->
                                        SPerms = scope_perms(ls_oauth:scope(Scope), []),
                                        State1 = State#wsstate{token = UUID, scope_perms = SPerms},
                                        {ok, Req3, State1}
                                end;
                            _ ->
                                {denied, Req3, State}
                    end;
                _ ->
                        {denied, Req3, State}
                end
        end,
    case R of
        {no_token, Req4, State} ->
            {ok, Req5} = cowboy_req:reply(401, [], <<"ott required">>, Req4),
            {shutdown, Req5};
        {denied, Req4, State} ->
            {ok, Req5} = cowboy_req:reply(403, [], <<"permission denied">>, Req4),
            {shutdown, Req5};
        R ->
            R
    end.


websocket_handle({Type, Raw}, Req,
                 State = #wsstate{type = Type, decoder = Dec, encoder = Enc}) ->
    case handle_data(Dec(Raw), State) of
        {reply, Reply, State1} ->
            {reply, {Type, Enc(Reply)}, Req, State1};
        {ok, State1} ->
            {ok, Req, State1};
        Reply ->
            Reply
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
                    SPerms = scope_perms(ls_oauth:scope(Scope), []),
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

scope_perms([], Acc) ->
    lists:usort(Acc);
scope_perms([{_, _, _, Perms} | R], Acc) ->
    scope_perms(R, Acc ++ Perms).
