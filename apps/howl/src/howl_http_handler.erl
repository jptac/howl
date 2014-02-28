-module(howl_http_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).

-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
              websocket_info/3, websocket_terminate/3]).
-ignore_xref([init/3, handle/2, terminate/2]).

-record(state, {token, encoder, decoder, type, channels=[]}).

init({_Andy, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, []) ->
    {ok, [C|_], Req0} =
        cowboy_req:parse_header(
          <<"sec-websocket-protocol">>,
          Req, [<<"json">>]),
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
                         {ok, O} = msgpack:unpack(D, [jsx]),
                         jsxd:from_list(O)
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
            <<>> ->
                {fun(O) ->
                         jsx:encode(O)
                 end,
                 fun(D) ->
                         jsxd:from_list(jsx:decode(D))
                 end, text, Req0}

        end,
    case cowboy_req:header(<<"x-snarl-token">>, Req2) of
        {<<Token:36/binary>>, Req3} ->
            {ok, Req3, #state{encoder = Encoder, decoder = Decoder,
                              type = Type, token = {token, Token}}};
        {_, Req3} ->
            case cowboy_req:cookie(<<"x-snarl-token">>, Req3) of
                {<<Token:36/binary>>, Req4} ->
                    {ok, Req4, #state{encoder = Encoder, decoder = Decoder,
                                      type = Type, token = {token, Token}}};
                {_, Req4} ->
                    {ok, Req4, #state{encoder = Encoder, decoder = Decoder,
                                      type = Type}}
            end
    end.

websocket_handle({Type, Raw}, Req,
                 State = #state{type = Type, decoder = Dec}) ->
    handle_data(Dec(Raw), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(normal, _Req, #state{channels = Cs}) ->
    [howl:leave(C) || C <- Cs],
    ok;

websocket_terminate(Reason, _Req, State) ->
    lager:warning("terminating websocket with reason ~p when state was ~p.",
                  [Reason, State]),
    ok.

handle_data([{<<"ping">>, V}], Req,
            State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc([{<<"pong">>, V}])}, Req, State};

handle_data([{<<"token">>, <<Token:36/binary>>}], Req,
            State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc([{<<"ok">>, <<"authenticated">>}])}, Req,
     State#state{token = {token, Token}}};

handle_data([{<<"token">>, _}], Req,
            State = #state{type = Type, encoder = Enc}) ->
    {reply, {Type, Enc([{<<"error">>, <<"invalid token">>}])}, Req,
     State};

handle_data([{<<"auth">>, Auth}], Req,
            State = #state{type = Type, encoder = Enc}) ->
    {ok, User} = jsxd:get([<<"user">>], Auth),
    {ok, Pass} = jsxd:get([<<"pass">>], Auth),
    case libsnarl:auth(User, Pass) of
        {ok, Token} ->
            {reply, {Type, Enc([{<<"ok">>, <<"authenticated">>}])}, Req,
             State#state{token =  Token}};
        _ ->
            {reply, {Type, Enc([{<<"error">>, <<"authentication failed">>}])},
             Req, State}
    end;

handle_data(_, Req, State = #state{type = Type,
                                   encoder = Enc, token = undefined}) ->
    {reply, {Type, Enc([{<<"error">>, <<"not authenticated">>}])}, Req, State};

handle_data([{<<"join">>, Channel}], Req,
            State = #state{token = Token, type = Type, encoder = Enc,
                           channels=Cs}) ->
    case libsnarl:allowed(Token, [<<"channels">>, Channel, <<"join">>]) of
        true ->
            howl:listen(Channel),
            {reply, {Type, Enc([{<<"ok">>, <<"channel joined">>}])},
             Req, State#state{channels=[Channel | Cs]}};
        _ ->
            {reply, {Type, Enc([{<<"error">>, <<"permission denied">>}])},
             Req, State}
    end;


handle_data([{<<"leave">>, Channel}], Req,
            State = #state{type = Type, encoder = Enc, channels=Cs}) ->
    howl:leave(Channel),
    {reply, {Type, Enc([{<<"ok">>, <<"channel left">>}])}, Req,
     State#state{channels=[C || C <- Cs, C =/= Channel]}};

handle_data(_JSON, Req, State) ->
    {ok, Req, State}.
