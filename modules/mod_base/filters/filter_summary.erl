%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Give a plaintext summary of the resource. Takes either the summary or, if non existent, a part of the body text.

%% Copyright 2010 Arjan Scherpenisse
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

-module(filter_summary).
-export([summary/2, summary/3]).

-include("zotonic.hrl").

summary(undefined, _Context) ->
    undefined;
summary(RId, Context) ->
    summary(RId, 200, Context).

summary(undefined, _N, _Context) ->
    undefined;
summary(RId, N, Context) ->
    Id = m_rsc:rid(RId, Context),
    S = case m_rsc:p(Id, summary, Context) of
            X when X =/= [] andalso X =/= <<>> andalso X =/= undefined ->
                X;
            _Empty ->
                Body = m_rsc:p(Id, body, Context),
                z_html:strip(Body)
    end,
    z_string:truncate(z_trans:lookup_fallback(S, Context), z_convert:to_integer(N)).
