%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Filter elements based on the value of a property.

%% Copyright 2026 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(filter_filter_by).
-export([filter_by/3, filter_by/4, filter_by/5]).

-define(IS_BUILTIN(P), (
                     (P == <<"eq">>) orelse (P == eq)
                     orelse (P == <<"ne">>) orelse (P == ne)
                     orelse (P == <<"gt">>) orelse (P == gt)
                     orelse (P == <<"ge">>) orelse (P == ge)
                     orelse (P == <<"lt">>) orelse (P == lt)
                     orelse (P == <<"le">>) orelse (P == le)
                     orelse (P == <<"between">>) orelse (P == between)
                     orelse (P == <<"in">>) orelse (P == in)
                     orelse (P == <<"not_in">>) orelse (P == not_in)
                    )
).

filter_by(undefined, _Key, _Context) ->
    undefined;
filter_by(List, Prop, Context) when is_list(List) ->
    F = fun(Elt) ->
                case extract_value(Elt, Prop, Context) of
                    undefined -> false;
                    _ -> true
                end
        end,
    lists:filter(F, List).

filter_by(undefined, _Prop, _Predicate, _Context) ->
    undefined;
filter_by(List, Prop, Predicate, Context) when is_list(List) ->
    F = predicate_fun(Prop, Predicate, [], Context),
    lists:filter(F, List).

filter_by(undefined, _Key, _Predicate, _Args, _Context) ->
    undefined;

filter_by(List, Prop, Predicate, Args, Context) when is_list(List) andalso ?IS_BUILTIN(Predicate) andalso is_list(Args) ->
    F = builtin_predicate_fun(Prop, Predicate, Args, Context),
    lists:filter(F, List);
filter_by(List, Prop, Predicate, Args, Context) when is_list(List) andalso is_list(Args) ->
    F = predicate_fun(Prop, Predicate, Args, Context),
    lists:filter(F, List);
filter_by(List, Key, Predicate, Arg, Context) when is_list(List) ->
    filter_by(List, Key, Predicate, [Arg], Context).

predicate_fun(Prop, Predicate, Args, Context) ->
    Module = filter_module(Predicate),
    Fun = z_convert:to_atom(Predicate),
    Args1 = Args ++ [Context],
    fun(Elt) ->
            Value = extract_value(Elt, Prop, Context),
            erlang:apply(Module, Fun, [Value | Args1])
    end.

builtin_predicate_fun(Prop, Predicate, Args, Context) ->
    P = normalize_builtin_predicate(Predicate),
    Args1 = [ z_template_compiler_runtime:to_simple_value(A, Context) || A <- Args ],
    fun(Elt) ->
        Value = extract_value(Elt, Prop, Context),
        builtin_predicate(Value, P, Args1, Context)
    end.

builtin_predicate(Value, between, [Lower, Upper], Context) ->
    operator(Value, ge, Lower, Context) andalso operator(Value, le, Upper, Context);
builtin_predicate(Value, in, List, Context) ->
    member(Value, List, Context);
builtin_predicate(Value, not_in, List, Context) ->
    not member(Value, List, Context);
builtin_predicate(Value, Op, [Other], Context) ->
    operator(Value, Op, Other, Context).

operator(Value, Op, Other, Context) ->
    template_compiler_operators:Op(Value, Other, z_template_compiler_runtime, Context).

member(_Value, [], _Context) ->
    false;
member(Value, [Other|Rest], Context) ->
    case operator(Value, eq, Other, Context) of
        true ->
            true;
        false ->
            member(Value, Rest, Context)
    end.

extract_value(Item, Prop, Context) ->
    z_template_compiler_runtime:find_value(Prop, Item, #{}, Context).

filter_module(Predicate) ->
    list_to_atom("filter_" ++ z_convert:to_list(Predicate)).

normalize_builtin_predicate(<<"eq">>) -> eq;
normalize_builtin_predicate(<<"ne">>) -> ne;
normalize_builtin_predicate(<<"gt">>) -> gt;
normalize_builtin_predicate(<<"ge">>) -> ge;
normalize_builtin_predicate(<<"lt">>) -> lt;
normalize_builtin_predicate(<<"le">>) -> le;
normalize_builtin_predicate(<<"between">>) -> between;
normalize_builtin_predicate(<<"in">>) -> in;
normalize_builtin_predicate(<<"not_in">>) -> not_in;
normalize_builtin_predicate(eq) -> eq;
normalize_builtin_predicate(ne) -> ne;
normalize_builtin_predicate(gt) -> gt;
normalize_builtin_predicate(ge) -> ge;
normalize_builtin_predicate(lt) -> lt;
normalize_builtin_predicate(le) -> le;
normalize_builtin_predicate(between) -> between;
normalize_builtin_predicate(in) -> in;
normalize_builtin_predicate(not_in) -> not_in;
normalize_builtin_predicate(_) -> eq.

