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
    case m_rsc:rid(RId, Context) of
        undefined ->
            undefined;
        Id ->
            S = get_summary_text(Id, Context),
            S1 = case z_utils:is_empty(S) of
                    true ->
                        Body = m_rsc:p(Id, body, Context),
                        z_string:trim_left(z_html:strip(z_trans:lookup_fallback(Body, Context)));
                    false ->
                        S
                end,
            z_string:trim(z_string:truncate(S1, z_convert:to_integer(N)))
    end.

    get_summary_text(Id, Context) ->
        case m_rsc:p(Id, summary, Context) of
            {trans, _} = T -> z_trans:lookup_fallback(T, Context);
            Other -> Other
        end.
