%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024-2025 Marc Worrell <marc@worrell.nl>
%% @doc Column expression and value functions for CSV/XLSX exports.
%% @end

%% Copyright 2024-2025 Marc Worrell
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

-module(export_value).

-export([
    rsc_props_default/1,
    maybe_rsc_props/2,
    rsc_props/2,
    prepare_rsc_props/2,
    header/1,
    value/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec rsc_props_default(z:context()) -> [ binary() ].
rsc_props_default(Context) ->
    (m_rsc:common_properties(Context) ++ [ <<"page_url_abs">> ]) -- [ <<"blocks">> ].


%% @doc Return the properties to export for a resource. The fields can be expressions, for
%% example: foo|default:"none"  These expressions can be evaluated using z_expression.
%% Return 'undefined' if the resource does not have props defined.
-spec maybe_rsc_props(Id, Context) -> Fields when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    Fields :: [ binary() ]
            | undefined.
maybe_rsc_props(Id, Context) ->
    case m_rsc:p(Id, <<"export_fields">>, Context) of
        undefined -> undefined;
        Fields when is_binary(Fields) ->
            case split_fields(z_html:unescape(Fields)) of
                [] -> undefined;
                Fs -> Fs
            end
    end.

%% @doc Return the properties to export for a resource. The fields can be expressions, for
%% example: foo|default:"none"  These expressions can be evaluated using z_expression.
-spec rsc_props(Id, Context) -> Fields when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    Fields :: [ binary() ].
rsc_props(Id, Context) ->
    case maybe_rsc_props(Id, Context) of
        undefined -> rsc_props_default(Context);
        Props -> Props
    end.

split_fields(Fields) ->
    All = binary:split(Fields, [ <<"\n">>, <<"\r">>, <<" ">>, <<"\t">> ], [ global, trim_all ]),
    All1 = [ z_string:trim(F) || F <- All ],
    [ F || F <- All1, F =/= <<>> ].

%% @doc Return the header name for the prop expression.
-spec header(Expr) -> Header when
    Expr :: {value, binary(), binary()}
          | {expr, binary(), z_expression:tree()}
          | {term(), binary()|string()}
          | term(),
    Header :: binary().
header({value, Name, _Value}) ->
    Name;
header({expr, Expr, _ExprTree}) ->
    % Take the expression before any filters or arguments.
    hd(binary:split(Expr, [ <<"|">>, <<"::">> ]));
header({_Key, Name}) ->
    Name;
header(NameOrValue) ->
    NameOrValue.


%% @doc Return the value for the prop expression.
-spec value(Id, Expr, Context) -> Value when
    Id :: m_rsc:resource_id(),
    Expr :: {value, binary(), binary()}
          | {expr, binary(), z_expression:tree()}
          | {term(), binary()|string()}
          | term(),
    Context :: z:context(),
    Value :: binary()
           | #trans{}
           | term().
value(_Id, {value, _Name, Value}, _Context) ->
    Value;
value(Id, {expr, _Name, Expr}, Context) ->
    Vars = #{
        <<"id">> => Id
    },
    z_expression:eval(Expr, Vars, Context);
value(Id, {Key, _Header}, Context) ->
    z_template_compiler_runtime:find_value(Key, Id, #{}, Context);
value(Id, Key, Context) ->
    z_template_compiler_runtime:find_value(Key, Id, #{}, Context).

% @doc Precompile the column expressions for the exports.
-spec prepare_rsc_props(Props, Context) -> PrepProps when
    Props :: [ Prop | {Prop, Name} ],
    Name :: atom()
          | binary()
          | string(),
    Prop :: atom()
          | binary()
          | string()
          | #trans{},
    Context :: z:context(),
    PrepProps :: [ PrepProp ],
    PrepProp:: {value, binary(), binary()}
             | {expr, binary(), Expr},
    Expr :: z_expression:tree().
prepare_rsc_props(Props, Context) ->
    lists:map(fun(P) -> prepare_rsc_prop(P, Context) end, Props).

prepare_rsc_prop({<<>>, Name}, Context) ->
    {value, to_bin(Name, Context), <<>>};
prepare_rsc_prop({Expr, Name}, Context) ->
    case z_expression:parse(Expr) of
        {ok, V} -> {expr, to_bin(Name, Context), V};
        {error, _} -> {value, to_bin(Name, Context), <<>>}
    end;
prepare_rsc_prop(<<>>, _Context) ->
    {value, <<>>, <<>>};
prepare_rsc_prop(Expr, _Context) when is_binary(Expr) ->
    case z_expression:parse(Expr) of
        {ok, V} -> {expr, Expr, V};
        {error, _} -> {value, Expr, <<>>}
    end;
prepare_rsc_prop(Expr, Context) ->
    prepare_rsc_prop(to_bin(Expr, Context), Context).

to_bin(#trans{} = Tr, Context) ->
    to_bin(z_trans:lookup_fallback(Tr, Context), Context);
to_bin(V, _Context) ->
    z_convert:to_binary(V).

