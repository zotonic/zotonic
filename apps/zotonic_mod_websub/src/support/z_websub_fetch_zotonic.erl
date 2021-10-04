%% @doc Fetch resource from remote Zotonic site, keep in sync.
%% @copyright 2021 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>

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

-module(z_websub_fetch_zotonic).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    fetch/3,
    fetch_json/1
]).


%% @doc Fetch the page at the URL, keep it in sync with the page that was
%% previously synchronized. Optionally fetches all objects of the page.
-spec fetch( Url, Options, Context ) -> {ok, {m_rsc:resource_id(), [ m_rsc:resource_id() ]}} | {error, term()}
    when Url :: binary(),
         Options :: list(),
         Context :: z:context().
fetch(Url, Options, Context) ->
    case fetch_json(Url) of
        {ok, JSON} ->
            import_json(Url, JSON, Options, Context);
        {error, _} = Error ->
            Error
    end.

% Example url:
%
% https://test.zotonic.com/id/1"
%
fetch_json(Url) ->
    Options = [
        {accept, "application/json"},
        {user_agent, "Zotonic-WebSub"},
        insecure
    ],
    case z_url_fetch:fetch(Url, Options) of
        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
            JSON = jsxrecord:decode(Body),
            {ok, JSON};
        {error, _} = Error ->
            lager:warning("WebSub: error fetching ~p: ~p", [Url, Error]),
            Error
    end.

import_json(_Url, #{<<"status">> := <<"ok">>, <<"result">> := JSON}, Options, Context) ->
    m_rsc_import:import(JSON, Options, Context);
import_json(Url, JSON, _Options, _Context) ->
    lager:warning("WebSub: JSON without status ok ~p: ~p", [Url, JSON]),
    {error, status}.

