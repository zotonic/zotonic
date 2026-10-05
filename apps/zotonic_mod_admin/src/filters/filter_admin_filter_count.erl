%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Count active filters in admin overview query arguments.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(filter_admin_filter_count).
-author("Marc Worrell <marc@worrell.nl>").
-moduledoc("Count nonempty overview filters, excluding sorting and pagination. Zero and false are active filter values.").

-export([admin_filter_count/2]).

%% @doc Count active query filters for the overview's Filter button badge.
-spec admin_filter_count(Args, Context) -> non_neg_integer() when
    Args :: [{binary(), term()}] | undefined, Context :: z:context().
admin_filter_count(undefined, _Context) ->
    0;
admin_filter_count(Args, _Context) when is_list(Args) ->
    length([Key || {Key, Value} <- Args,
        Value =/= undefined, Value =/= <<>>, Value =/= [],
        not lists:member(Key, [<<"qsort">>, <<"qzsort">>, <<"qpage">>, <<"qpagelen">>, <<"qargs">>])]).
