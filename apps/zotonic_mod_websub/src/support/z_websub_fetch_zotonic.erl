%% @copyright 2021-2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Fetch resource from remote Zotonic site, keep in sync.
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(z_websub_fetch_zotonic).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    fetch/3,
    fetch_json/1,
    fetch_json/2
]).


%% @doc Fetch the page at the URL, keep it in sync with the page that was
%% previously synchronized. Optionally fetches all objects of the page.
-spec fetch(Url, Options, Context) -> {ok, {m_rsc:resource_id(), [m_rsc:resource_id()]}} | {error, term()}
    when Url :: binary(),
         Options :: list(),
         Context :: z:context().
fetch(Url, Options, Context) ->
    case fetch_json(Url, Context) of
        {ok, JSON} ->
            import_json(Url, JSON, Options, Context);
        {error, _} = Error ->
            Error
    end.

fetch_json(Url) ->
    fetch_json(Url, z_acl:anondo(z_context:new(default))).

fetch_json(Url, Context) ->
    Options = [
        {accept, <<"application/json">>},
        {user_agent, <<"Zotonic-WebSub">>}
    ],
    case z_fetch:fetch_json(Url, Options, Context) of
        {ok, JSON} ->
            {ok, JSON};
        {error, {Code, _FinalUrl, _Hs, _Size, _Body}} when Code =:= 401; Code =:= 403 ->
            {error, eacces};
        {error, {404, _FinalUrl, _Hs, _Size, _Body}} ->
            {error, enoent};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_websub,
                text => <<"WebSub: error fetching url">>,
                result => error,
                reason => Reason,
                url => Url
            }),
            Error
    end.

import_json(_Url, #{<<"status">> := <<"ok">>, <<"result">> := JSON}, Options, Context) ->
    m_rsc_import:import(JSON, Options, Context);
import_json(Url, JSON, _Options, _Context) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_websub,
        text => <<"WebSub: JSON without status ok">>,
        result => error,
        reason => status,
        url => Url,
        json => JSON
    }),
    {error, status}.
