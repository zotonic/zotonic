%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Update tasks to be triggered by the z_install_update.erl change routines.

%% Copyright 2021 Marc Worrell
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

-module(z_install_update_task).

-author("Marc Worrell <marc@worrell.nl").

-export([
    init_language/1,
    init_language/2
]).

-include_lib("../../include/zotonic.hrl").

%% @doc Initialize the language column from the language property in the
%% serialized props.
-spec init_language( z:context() ) -> ok | {delay, integer(), list()}.
init_language(Context) ->
    case z_db:q1("select max(id) from rsc", Context) of
        undefined ->
            ok;
        Id ->
            init_language(Id, Context)
    end.

-spec init_language( undefined | integer(), z:context() ) -> ok | {delay, integer(), list()}.
init_language(Id, Context) ->
    case z_db:transaction(
        fun(Ctx) ->
            init_language_trans(Id, Ctx)
        end,
        Context)
    of
        {ok, Id1} when Id1 < 1 ->
            ok;
        {ok, Id1} ->
            {delay, 0, [ Id1 ]};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error during fixing languages, delaying next batch">>,
                in => zotonic_core,
                rsc_id => Id,
                result => error,
                reason => Reason
            }),
            {delay, 3600, [ Id ]}
    end.

init_language_trans(MaxId, Context) ->
    Rs = z_db:q("select id, props from rsc where id <= $1 order by id desc limit 1000", [ MaxId ], Context),
    lists:map(
        fun({Id, Props}) ->
            Language = language(Props),
            Language1 = [ z_convert:to_binary(Iso) || Iso <- Language ],
            z_db:q("
                update rsc set language = $1 where id = $2",
                [ Language1, Id ],
                Context)
        end,
        Rs),
    case Rs of
        [] -> {ok, -1};
        _ -> {ok, lists:min( [ Id || {Id, _} <- Rs ]) - 1}
    end.

language(#{ <<"language">> := Language }) when is_list(Language) ->
    Language;
language(Props) when is_list(Props) ->
    case proplists:get_value(language, Props, []) of
        L when is_list(L) -> L;
        _ -> []
    end;
language(_) ->
    [].
