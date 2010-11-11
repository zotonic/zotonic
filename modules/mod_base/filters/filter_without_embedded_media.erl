%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc 'show_media' filter, show the media inserted with the html editor.

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

-module(filter_without_embedded_media).
-export([without_embedded_media/3]).

-include_lib("zotonic.hrl").


without_embedded_media(undefined, _Id, _Context) ->
    undefined;
without_embedded_media(Input, Id, Context) ->
    case erlydtl_runtime:to_list(Input, Context) of
        [] -> [];
        Ids ->
            Body = z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(Id, body, Context), Context)),
            case re:run(Body, "\<\!-- z-media ([0-9]+) ", [global, {capture, all_but_first, list}]) of
                nomatch -> Ids;
                {match, L} ->
                    S = [z_convert:to_integer(I) || [I] <- L],
                    lists:filter(fun(I) -> not(lists:member(I, S)) end, Ids)
            end
    end.
