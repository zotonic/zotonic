%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_runtime).
-compile(export_all).

-include_lib("zotonic.hrl").


% Find the value of a model value
find_value(<<>>, #m{}, _Context) ->
    undefined;
find_value(undefined, #m{}, _Context) ->
    undefined;
find_value(Key, #m{model=Model} = M, Context) ->
    Model:m_find_value(Key, M, Context);

find_value(Key, [{Nr,_}|_] = L, _Context) when is_integer(Key), is_integer(Nr) ->
    proplists:get_value(Key, L);
    
% Index of list with an integer like "a[2]"
find_value(Key, L, _Context) when is_integer(Key), is_list(L) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;

%% Assume a predicate/property lookup in a list of ids, map to lookup of first entry
find_value(Key, [N|_], Context) when is_atom(Key), is_integer(N) ->
    m_rsc:p(N, Key, Context);

%% Property of a resource, just assume an integer is a rsc id
find_value(Key, Id, Context) when is_atom(Key), is_integer(Id) ->
    m_rsc:p(Id, Key, Context);

%% Property of a resource, just assume an integer is a rsc id
find_value(Key, RscName, Context) when is_atom(Key), is_atom(RscName) ->
    m_rsc:p(RscName, Key, Context);

%% List of proplists - blocks in the rsc
find_value(Name, [[{A,_}|_]|_] = Blocks, _Context ) when is_atom(A), not is_integer(Name) ->
    NameB = z_convert:to_binary(Name),
    case lists:dropwhile(fun(Ps) -> proplists:get_value(name, Ps) =/= NameB end, Blocks) of
        [] -> undefined;
        [Block|_] -> Block
    end;

%% Regular proplist lookup
find_value(Key, L, _Context) when is_list(L) ->
    proplists:get_value(Key, L);

%% Resource list handling, special lookups when skipping the index
find_value(Key, #rsc_list{list=L}, _Context) when is_integer(Key) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, #rsc_list{list=[H|_T]}, Context) ->
    find_value(Key, H, Context);
find_value(_Key, #rsc_list{list=[]}, _Context) ->
    undefined;

%% Translations lookup
find_value(IsoAtom, {trans, Tr}, _Context) ->
    proplists:get_value(IsoAtom, Tr, <<>>);
find_value(IsoAtom, Text, _Context) when is_atom(IsoAtom), is_binary(Text) ->
    case z_trans:is_language(atom_to_list(IsoAtom)) of
        true ->
            Text;
        false ->
            undefined
    end;

%% JSON-decoded proplist structure
find_value(Key, {obj, Props}, _Context) ->
    proplists:get_value(z_convert:to_list(Key), Props);

% Index of tuple with an integer like "a[2]"
find_value(Key, T, _Context) when is_integer(Key) andalso is_tuple(T) ->
    case element(1,T) of
        dict ->
            case dict:find(Key, T) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        _ ->
            try
                element(Key, T)
            catch 
                _:_ -> undefined
            end
    end;

% Search results
find_value(Key, #search_result{} = S, _Context) when is_integer(Key) ->
    try
        lists:nth(Key, S#search_result.result)
    catch
        _:_ -> undefined
    end;
find_value(Key, #search_result{} = S, _Context) ->
    case Key of
        result -> S#search_result.result;
        all -> S#search_result.all;
        total -> S#search_result.total;
        page -> S#search_result.page;
        pages -> S#search_result.pages
    end;

% gbtree lookup
find_value(Key, {GBSize, GBData}, _Context) when is_integer(GBSize) ->
    case gb_trees:lookup(Key, {GBSize, GBData}) of
        {value, Val} -> Val;
        _ -> undefined
    end;

%% Other cases: context, dict or parametrized module lookup.
find_value(Key, Tuple, Context) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        context -> 
            z_context:get_value(Key, Tuple);
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} -> Val;
                _ -> undefined
            end;
        Module ->
            Exports = Module:module_info(exports),
            case proplists:get_value(Key, Exports) of
                0 -> Tuple:Key();
                1 -> Tuple:Key(Context);
                _ ->
                    case proplists:get_value(get, Exports) of
                        1 -> Tuple:get(Key);
                        2 -> Tuple:get(Key, Context);
                        _ ->
                            undefined
                    end
            end
    end;

%% When the current value lookup is a function, the context can be passed to F
find_value(Key, F, Context) when is_function(F, 2) ->
	F(Key, Context);
find_value(Key, F, _Context) when is_function(F, 1) ->
	F(Key);

%% Any subvalue of a non-existant value is undefined
find_value(_Key, undefined, _Context) ->
    undefined;
find_value(_Key, <<>>, _Context) ->
    undefined.

%% This used to translate undefined into <<>>, this translation is now done by z_render:render/2
fetch_value(Key, Data, Context) ->
    find_value(Key, Data, Context).

are_equal(Arg1, Arg2) ->
    z_utils:are_equal(Arg1, Arg2).
    
is_false({trans, _} = T, Context) ->
    z_utils:is_empty(z_trans:lookup_fallback(T, Context));
is_false(A, _Context) ->
    is_false(A).

is_false(A) ->
    not z_convert:to_bool(A).


is_true({trans, _} = T, Context) ->
    not z_utils:is_empty(z_trans:lookup_fallback(T, Context));
is_true(A, _Context) ->
    is_true(A).

is_true(A) ->
    z_convert:to_bool(A).

init_counter_stats(List) ->
    init_counter_stats(List, undefined).

init_counter_stats(List, Parent) ->
    N = length(List),
    [{counter, 1}, 
        {counter0, 0}, 
        {revcounter, N}, 
        {revcounter0, N - 1}, 
        {first, true}, 
        {last, N =:= 1},
        {parentloop, Parent}].


to_list(#m{model=Model} = M, Context) -> Model:m_to_list(M, Context);
to_list(#rsc_list{list=L}, _Context) -> L;
to_list(#search_result{result=L}, _Context) -> L;
to_list(#m_search_result{result=Result}, Context) -> to_list(Result, Context);
to_list(q, Context) -> z_context:get_q_all(Context);
to_list(q_validated, _Context) -> [];
to_list({trans, _} = Trans, Context) -> to_list(z_trans:lookup_fallback(Trans, Context), Context);
to_list(L, _Context) when is_list(L) -> L;
to_list(T, _Context) when is_tuple(T) -> tuple_to_list(T);
to_list(N, _Context) -> z_convert:to_list(N).

to_value(#m{model=Model} = M, Context) ->
    Model:m_value(M, Context);
to_value(#m_search_result{result=#search_result{result=Result}}, _Context) ->
    Result;
to_value(V, _Context) ->
    V.
    
increment_counter_stats([{counter, Counter}, {counter0, Counter0}, {revcounter, RevCounter},
         {revcounter0, RevCounter0}, {first, _}, {last, _}, {parentloop, Parent}]) ->
    [{counter, Counter + 1},
        {counter0, Counter0 + 1},
        {revcounter, RevCounter - 1},
        {revcounter0, RevCounter0 - 1},
        {first, false}, {last, RevCounter0 =:= 1},
        {parentloop, Parent}].


cycle(NamesTuple, Counters, Context) when is_tuple(NamesTuple) ->
    element(fetch_value(counter0, Counters, Context) rem size(NamesTuple) + 1, NamesTuple).


spaceless(Contents) ->
    Contents1 = re:replace(Contents, "^[ \t\n\f\r]+<", "<"),
    Contents2 = re:replace(Contents1, ">[ \t\n\f\r]+$", ">"),
    Contents3 = re:replace(Contents2, ">[ \t\n\f\r]+<", "><", [global]),
    Contents3.

cache(MaxAge, Name, Args, Func, Context) ->
    VisibleFor = z_acl:args_to_visible_for(Args),
    case do_cache(VisibleFor, Args, Context) of
        false ->
            Func(Context);
        true ->
            Varies = lists:flatten(proplists:get_all_values(vary, Args)),
            Cat = proplists:get_all_values(cat, Args),
            Cat1 = lists:map(fun z_convert:to_atom/1, Cat),
            FuncContext = z_acl:set_visible_for(VisibleFor, Context),
            Key = {Name, Varies, z_acl:cache_key(FuncContext)},
            F = fun() ->
                Func(FuncContext)
            end,
            z_depcache:memo(F, Key, MaxAge, Varies ++ Cat1, FuncContext)
    end.

do_cache(VisibleFor, Args, Context) ->
    do_cache1(get_bool_value('if', Args, true), VisibleFor, Args, Context).

do_cache1(true, ?ACL_VIS_PUBLIC, _Args, _Context) ->
    true;
do_cache1(true, _VisibleFor, Args, Context) ->
    case get_bool_value(if_anonymous, Args, false) of
	true -> z_acl:user(Context) =:= undefined;
	false -> true
    end;
do_cache1(false, _VisibleFor, _Args, _Context) ->
    false.

get_bool_value(Key, Args, Default) ->
    z_convert:to_bool(proplists:get_value(Key, Args, Default)).
