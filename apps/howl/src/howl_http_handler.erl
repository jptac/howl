-module(howl_http_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
              websocket_info/3, websocket_terminate/3]).
-ignore_xref([init/3, handle/2, terminate/2]).

-record(state, {token, encoder, decoder, type}).

init({_Any, http}, Req, []) ->
    case cowboy_req:header(<<"Upgrade">>, Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req1} =  cowboy_req:reply(200, [], <<"">>, Req),
    {ok, Req1, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    {_, C, Req0} = cowboy_req:parse_header(<<"Sec-Websocket-Protocol">>, Req, <<"json">>),
    Req1 = cowboy_req:compact(Req0),
    {Encoder, Decoder, Type} = case C of
                                   <<"msgpack">> ->
                                       {fun(O) ->
                                                msgpack:pack(O, [jsx])
                                        end,
                                        fun(D) ->
                                                {ok, O} = msgpack:unpack(D, [jsx]),
                                                jsxd:from_list(O)
                                        end,
                                        binary};
                                   <<"json">> ->
                                       {fun(O) ->
                                                jsx:encode(O)
                                        end,
                                        fun(D) ->
                                                jsxd:from_list(jsx:decode(D))
                                        end, text};
                                   <<>> ->
                                       {fun(O) ->
                                                jsx:encode(O)
                                        end,
                                        fun(D) ->
                                                jsxd:from_list(jsx:decode(D))
                                        end, text}

                               end,
    case cowboy_req:header(<<"X-Snarl-Token">>, Req1) of
        {undefined, Req2} ->
            case cowboy_req:cookie(<<"X-Snarl-Token">>, Req2) of
                {undefined, Req3} ->
                    {ok, Req3, #state{encoder = Encoder, decoder = Decoder, type = Type}};
                {Token, Req3} ->
                    {ok, Req3, #state{encoder = Encoder, decoder = Decoder, type = Type, token = {token, Token}}}
            end;
        {Token, Req2} ->
            {ok, Req2, #state{encoder = Encoder, decoder = Decoder, type = Type, token = {token, Token}}}
    end.

websocket_handle({Type, Raw}, Req, State = #state{type = Type, decoder = Dec}) ->
    handle_data(Dec(Raw), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(normal, _Req, _State) ->
    ok;

websocket_terminate(Reason, _Req, State) ->
    lager:warning("terminating websocket with reason ~p when state was ~p.", [Reason, State]),
    ok.

handle_data([{<<"ping">>, V}], Req, State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc([{<<"pong">>, V}])}, Req, State};

handle_data([{<<"token">>, Token}], Req, State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc([{<<"ok">>, <<"authenticated">>}])}, Req, State#state{token = {token, Token}}};

handle_data([{<<"auth">>, Auth}], Req, State = #state{type = Type, encoder = Enc}) ->
    {ok, User} = jsxd:get([<<"user">>], Auth),
    {ok, Pass} = jsxd:get([<<"pass">>], Auth),
    case libsnarl:auth(User, Pass) of
        {ok, Token} ->
            {reply, {Type, Enc([{<<"ok">>, <<"authenticated">>}])}, Req, State#state{token =  Token}};
        _ ->
            {reply, {Type, Enc([{<<"error">>, <<"authentication failed">>}])}, Req, State}
    end;

handle_data(_, Req, State = #state{type = Type, encoder = Enc, token = undefined}) ->
    {reply, {Type, Enc([{<<"error">>, <<"not authenticated">>}])}, Req, State};

handle_data([{<<"join">>, Channel}], Req, State = #state{token = Token, type = Type, encoder = Enc}) ->
    case libsnarl:allowed(Token, [<<"channels">>, Channel, <<"join">>]) of
        true ->
            howl:listen(Channel),
            {reply, {Type, Enc([{<<"ok">>, <<"channel joined">>}])}, Req, State};
        _ ->
            {reply, {Type, Enc([{<<"error">>, <<"permission denied">>}])}, Req, State}
    end;


handle_data([{<<"leave">>, Channel}], Req, State = #state{token = Token, type = Type, encoder = Enc}) ->
    case libsnarl:allowed(Token, [<<"channels">>, Channel, <<"join">>]) of
        true ->
            howl:leave(Channel),
            {reply, {Type, Enc([{<<"ok">>, <<"channel left">>}])}, Req, State};
        _ ->
            {reply, {Type, Enc([{<<"error">>, <<"permission denied">>}])}, Req, State}
    end;


handle_data(_JSON, Req, State) ->
    {ok, Req, State}.
