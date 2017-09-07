%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc 'linebreaksbr' filter, translate linebreaks into <br/> elements

%% Copyright 2012 Marc Worrell
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

-module(filter_linebreaksbr).
-export([linebreaksbr/2]).

linebreaksbr(S, Context) ->
    z_html:nl2br(to_binary(S, Context)).

to_binary(S, Context) ->
    try
        z_convert:to_binary(S, Context)
    catch
        _:_ ->
            unicode:characters_to_binary(S)
    end.
