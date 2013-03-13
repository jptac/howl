-module(howl_http_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
              websocket_info/3, websocket_terminate/3]).

-record(state, {token, encoder, decoder, type}).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req1} =  cowboy_http_req:reply(200, [], <<"">>, Req),
    {ok, Req1, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    {_, C, Req1} = cowboy_http_req:parse_header(<<"Sec-Websocket-Protocol">>, Req, <<"json">>),
    Req2 = cowboy_http_req:compact(Req1),
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
                                  end, text}
                         end,
    case cowboy_http_req:header(<<"X-Snarl-Token">>, Req2) of
        {undefined, Req3} ->
            case cowboy_http_req:cookie(<<"X-Snarl-Token">>, Req3) of
                {false, Req4} ->
                    {ok, Req4, #state{encoder = Encoder, decoder = Decoder, type = Type}};
                {Token, Req4} ->
                    {ok, Req4, #state{encoder = Encoder, decoder = Decoder, type = Type, token = Token}}
            end;
        {Token, Req3} ->
            {ok, Req3, #state{encoder = Encoder, decoder = Decoder, type = Type, token = Token}}
    end.

websocket_handle({Type, Raw}, Req, State = #state{type = Type, decoder = Dec}) ->
    handle_data(Dec(Raw), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc(Msg)}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle_data([{<<"ping">>, V}], Req, State) ->
    {reply, {text, jsx:encode([{<<"pong">>, V}])}, Req, State};

handle_data([{<<"token">>, Token}], Req, _State) ->
    {reply, {text, jsx:encode([{<<"ok">>, <<"authenticated">>}])}, Req, {token, Token}};

handle_data([{<<"auth">>, Auth}], Req, _State) ->
    {<<"user">>, User} = lists:keyfind(<<"user">>, 1, Auth),
    {<<"pass">>, Pass} = lists:keyfind(<<"pass">>, 1, Auth),
    case libsnarl:auth(User, Pass) of
        {ok, Token} ->
            {reply, {text, jsx:encode([{<<"ok">>, <<"authenticated">>}])}, Req, Token};
        _ ->
            {reply, {text, jsx:encode([{<<"error">>, <<"authentication failed">>}])}, Req, undefined}
    end;

handle_data(_, Req, undefined) ->
    {reply, {text, jsx:encode([{<<"error">>, <<"not authenticated">>}])}, Req, undefined};

handle_data([{<<"join">>, Channel}], Req, Token) ->
    case libsnarl:allowed(Token, [<<"channels">>, Channel, <<"join">>]) of
        true ->
            howl:listen(Channel),
            {reply, {text, jsx:encode([{<<"ok">>, <<"channel joined">>}])}, Req, Token};
        _ ->
            {reply, {text, jsx:encode([{<<"error">>, <<"permission denied">>}])}, Req, Token}
    end;

handle_data(_JSON, Req, State) ->
    {ok, Req, State}.
