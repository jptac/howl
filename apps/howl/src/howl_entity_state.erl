-module(howl_entity_state).

-export([
	 new/0,
	 add/2,
	 remove/2
	]).

new() ->
    ordsets:new().

add(Listener, Listeners) ->
    ordsets:add_element(Listener, Listeners).

remove(Listener, Listeners) ->
    ordsets:del_element(Listener, Listeners).
