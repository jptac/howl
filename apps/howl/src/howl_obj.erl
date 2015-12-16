%% @doc A suite of functions that operate on the algebraic data type
%% `howl_obj'.
%%
%% TODO Possibly move type/record defs in there and use accessor funs
%% and opaque types.
%%
%% Taken form https://github.com/Licenser/try-try-try/blob/master/2011/
%% riak-core-conflict-resolution/rts/src/rts_obj.erl

-module(howl_obj).
-export([ancestors/1, children/1, equal/1, equal/2, merge/1, unique/1,
         update/3]).
-export([val/1, vclock/1]).

-ignore_xref([
              ancestors/1,
              equal/1,
              unique/1,
              vclock/1
             ]).

-include("howl.hrl").

-type howl_obj() :: #howl_obj{}.
-type maybe_howl_obj() :: howl_obj() | not_found.
-export_type([howl_obj/0, maybe_howl_obj/0]).

%% @pure
%%
%% @doc Given a list of `howl_obj()' return a list of all the
%% ancestors.  Ancestors are objects that all the other objects in the
%% list have descent from.
-spec ancestors([maybe_howl_obj()]) -> [howl_obj()].
ancestors(Objs0) ->
    Objs = [O || O <- Objs0, O /= not_found],
    As = [[O2 || O2 <- Objs,
                 ancestor(O2#howl_obj.vclock,
                          O1#howl_obj.vclock)] || O1 <- Objs],
    unique(lists:flatten(As)).

%% @pure
%%
%% @doc Predicate to determine if `Va' is ancestor of `Vb'.
-spec ancestor(vclock:vclock(), vclock:vclock()) -> boolean().
ancestor(Va, Vb) ->
    vclock:descends(Vb, Va) andalso (vclock:descends(Va, Vb) == false).

%% @pure
%%
%% @doc Given a list of `howl_obj()' return a list of the children
%% objects.  Children are the descendants of all others objects.
children(Objs) ->
    unique(Objs) -- ancestors(Objs).

%% @pure
%%
%% @doc Predeicate to determine if `ObjA' and `ObjB' are equal.
-spec equal(ObjA::maybe_howl_obj(), ObjB::maybe_howl_obj()) -> boolean().
equal(#howl_obj{vclock=A}, #howl_obj{vclock=B}) -> vclock:equal(A, B);
equal(not_found, not_found) -> true;
equal(_, _) -> false.

%% @pure
%%
%% @doc Closure around `equal/2' for use with HOFs (damn verbose
%% Erlang).
-spec equal(ObjA::maybe_howl_obj()) ->
                   fun((ObjB::maybe_howl_obj()) -> boolean()).
equal(ObjA) ->
    fun(ObjB) -> equal(ObjA, ObjB) end.

%% @pure
%%
%% @doc Merge the list of `Objs', calling the appropriate reconcile
%% fun if there are siblings.
-spec merge([maybe_howl_obj()]) -> maybe_howl_obj().
merge([not_found|_]=Objs) ->
    P = fun(X) -> X == not_found end,
    case lists:all(P, Objs) of
        true -> not_found;
        false -> merge(lists:dropwhile(P, Objs))
    end;

merge([#howl_obj{}|_]=Objs) ->
    case howl_obj:children(Objs) of
        [] -> not_found;
        [Child] -> Child;
        Chldrn ->
            Val = howl_entity_read_fsm:reconcile(lists:map(fun val/1, Chldrn)),
            MergedVC = vclock:merge(lists:map(fun vclock/1, Chldrn)),
            #howl_obj{val=Val, vclock=MergedVC}
    end.

%% @pure
%%
%% @doc Given a list of `Objs' return the list of uniques.
-spec unique([maybe_howl_obj()]) -> [maybe_howl_obj()].
unique(Objs) ->
    F = fun(not_found, Acc) ->
                Acc;
           (Obj, Acc) ->
                case lists:any(equal(Obj), Acc) of
                    true -> Acc;
                    false -> [Obj|Acc]
                end
        end,
    lists:foldl(F, [], Objs).

%% @pure
%%
%% @doc Given a `Val' update the `Obj'.  The `Updater' is the name of
%% the entity performing the update.
-spec update(val(), node(), howl_obj()) -> howl_obj().
update(Val, Updater, #howl_obj{vclock=VClock0}=Obj0) ->
    VClock = vclock:increment(Updater, VClock0),
    Obj0#howl_obj{val=Val, vclock=VClock}.

-spec val(howl_obj()) -> any().
val(#howl_obj{val=Val}) -> Val;
val(not_found) -> not_found.

%% @pure
%%
%% @doc Given a vclock type `Obj' retrieve the vclock.
-spec vclock(howl_obj()) -> vclock:vclock().
vclock(#howl_obj{vclock=VC}) -> VC.
