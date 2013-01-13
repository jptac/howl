-module(howl_http_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

-ignore_xref([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(
		   200, [{'Content-Type', <<"text/html">>}],

%%% HTML code taken and adepted from cowboy example file which took it from the  misultin's example file.
		   <<"
<html>
  <head>
    <script type='text/javascript'>
    var ws;
    function addMsg(text){
      var content = document.getElementById('content');
      content.innerHTML = content.innerHTML + text + '<br/>';
      content.lastChild.scrollIntoView();
    }
    function send(input, event) {
      if (event.keyCode == 13) {
        ws.send(JSON.stringify({join: input.value}));
        input.value = '';
      }
    }
    function auth(pass, event) {
      if (event.keyCode == 13) {
        var user = document.getElementById('user');
        ws.send(JSON.stringify(
         {auth: {'user': user.value, 'pass': pass.value}}
        ));
       user.value = '';
       pass.value = '';
      }
    }

    function ready(){
      if ('MozWebSocket' in window) {
        WebSocket = MozWebSocket;
      }
      if ('WebSocket' in window) {
        // browser supports websockets
        ws = new WebSocket('ws://' + window.location.host + '/');
        ws.onopen = function() {
          addMsg('websocket connected!');
        };
        ws.onmessage = function (evt) {
          var receivedMsg = evt.data;
          addMsg(receivedMsg);
        };
        ws.onclose = function() {
          // websocket was closed
          addMsg('websocket was closed');
        };
      } else {
        // browser does not support websockets
        addStatus('sorry, your browser does not support websockets.');
      }
    }
    </script>
  </head>
  <body onload='ready();'>
  <div id='content' style='overflow:scroll;height:90%'></div>

  <div id='login'>
    <label>auth</label>
    <input id='user' type='text' onkeyup='auth(this, event);'/>
    <input type='text' onkeyup='auth(this, event);'/>
  </div>

  <div id='input'>
    <label>Channel</label>
    <input type='text' onkeyup='send(this, event);'/>
  </div>
  </body>
</html>">>,Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    case cowboy_http_req:cookie(<<"X-Snarl-Token">>, Req2) of
        {false, Req3} ->
            {ok, Req3, undefiend, hibernate};
        {Token, Req3} ->
            {ok, Req3, {token, Token}, hibernate}
    end.

websocket_handle({text, Raw}, Req, State) ->
    handle_json(jsx:decode(Raw), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
    {reply, {text, jsx:encode(Msg)}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle_json([{<<"ping">>, V}], Req, State) ->
    {reply, {text, jsx:encode([{<<"pong">>, V}])}, Req, State};

handle_json([{<<"token">>, Token}], Req, _State) ->
    {reply, {text, jsx:encode([{<<"ok">>, <<"authenticated">>}])}, Req, {token, Token}};

handle_json([{<<"auth">>, Auth}], Req, _State) ->
    {<<"user">>, User} = lists:keyfind(<<"user">>, 1, Auth),
    {<<"pass">>, Pass} = lists:keyfind(<<"pass">>, 1, Auth),
    case libsnarl:auth(User, Pass) of
	{ok, Token} ->
	    {reply, {text, jsx:encode([{<<"ok">>, <<"authenticated">>}])}, Req, Token};
	_ ->
	    {reply, {text, jsx:encode([{<<"error">>, <<"authentication failed">>}])}, Req, undefined}
    end;

handle_json(_, Req, undefined) ->
    {reply, {text, jsx:encode([{<<"error">>, <<"not authenticated">>}])}, Req, undefined};

handle_json([{<<"join">>, Channel}], Req, Token) ->
    case libsnarl:allowed(Token, [<<"channel">>, Channel, <<"join">>]) of
        true ->
	    howl:listen(Channel),
	    {reply, {text, jsx:encode([{<<"ok">>, <<"channel joined">>}])}, Req, Token};
	_ ->
	    {reply, {text, jsx:encode([{<<"error">>, <<"permission denied">>}])}, Req, Token}
    end;

handle_json(_JSON, Req, State) ->
    {ok, Req, State}.
