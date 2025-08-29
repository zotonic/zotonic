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
    rsc_props/1,
    rsc_props/2,
    prepare_rsc_props/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec rsc_props(z:context()) -> [ binary() ].
rsc_props(Context) ->
    m_rsc:common_properties(Context)
    ++ [ <<"page_url_abs">> ]
    -- [ <<"blocks">> ].

-spec rsc_props(m_rsc:resource_id(), z:context()) -> [ binary() ].
rsc_props(QueryId, Context) ->
    Props = z_convert:to_binary(m_rsc:p(QueryId, <<"export_fields">>, Context)),
    case binary:split(Props, [ <<";">>, <<",">>, <<"\n">>, <<"\r">>, <<"\t">>, <<" ">> ], [ global, trim_all ]) of
        [] -> rsc_props(Context);
        L -> L
    end.

% @doc Precompile the column expressions for the exports.
-spec prepare_rsc_props(Props, Context) -> PrepProps when
    Props :: [ Prop | {Name, Prop} ],
    Name :: atom()
          | binary()
          | string(),
    Prop :: atom()
          | binary()
          | string()
          | #trans{},
    Context :: z:context(),
    PrepProps :: [ PrepProp ],
    PrepProp:: {named, Name, Expr}
          | {value, binary(), binary()}
          | {expr, binary(), Expr},
    Expr :: z_expression:tree().
prepare_rsc_props(Props, Context) ->
    lists:map(fun(P) -> prepare_rsc_prop(P, Context) end, Props).

prepare_rsc_prop({Expr, Name}, Context) ->
    Expr1 = prepare_rsc_prop(Expr, Context),
    {named, to_bin(Name, Context), Expr1};
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

