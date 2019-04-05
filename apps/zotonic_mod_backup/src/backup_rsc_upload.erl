%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2018 Marc Worrell
%% @doc Handle generic upload of encoded blobs with a resource (from deprecated mod_rest)
%% @todo Should be replaced with m_rsc:m_post/3 handling

%% Copyright 2010-2018 Marc Worrell
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
-module(backup_rsc_upload).

-export([
    rsc_upload/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

rsc_upload(#rsc_upload{id=Id, format=bert, data=Data}, Context) ->
    case catch bert:decode(Data) of
        [ {OtherId, Props} ] when is_list(Props), is_integer(OtherId) ->
            rsc_upload(Id, Props, Context);
        [ {Prop,_} | _ ] = Props when is_list(Props), is_atom(Prop) ->
            rsc_upload(Id, Props, Context);
        _ ->
            {error, badarg}
    end.

rsc_upload(undefined, Props, Context) ->
    m_rsc_update:insert(update_props(Props, Context), [{escape_texts, false}, is_import], Context);
rsc_upload(Id, Props, Context) when is_integer(Id) ->
    m_rsc_update:update(Id, update_props(Props, Context), [{escape_texts, false}, is_import], Context).

update_props(Props, Context) ->
    UpdateProps = lists:filter(fun({K,_}) ->
                                    is_updateable(z_convert:to_binary(K))
                               end,
                               Props),
    [{category, map_category(Props, Context)} | UpdateProps ].

map_category(Props, Context) ->
    case proplists:get_value(computed_category, Props) of
        undefined ->
            other;
        List ->
            case lists:dropwhile(fun(Name) ->
                                    case m_category:name_to_id(Name, Context) of
                                        {ok, _} -> false;
                                        {error, _} -> true
                                    end
                                 end,
                                 List) of
                [] -> other;
                [N|_] -> N
            end
    end.

is_updateable(<<"id">>) -> false;
is_updateable(<<"category_id">>) -> false;
is_updateable(<<"category">>) -> false;
is_updateable(<<"version">>) -> false;
is_updateable(<<"medium">>) -> false;
is_updateable(<<"created">>) -> false;
is_updateable(<<"creator_id">>) -> false;
is_updateable(<<"modified">>) -> false;
is_updateable(<<"modifier_id">>) -> false;
is_updateable(<<"page_url">>) -> false;
is_updateable(<<"props">>) -> false;
is_updateable(<<"computed_", _/binary>>) -> false;
is_updateable(<<"pivot_", _/binary>>) -> false;
is_updateable(_) -> true.
