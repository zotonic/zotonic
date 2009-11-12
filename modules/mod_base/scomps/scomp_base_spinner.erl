%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Show the spinner element

%% Copyright 2009 Marc Worrell
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

-module(scomp_base_spinner).
-behaviour(gen_scomp).

-export([init/1, varies/2, terminate/2, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
terminate(_State, _Context) -> ok.

render(Params, _Vars, _Context, _State) ->
    Image = proplists:get_value(image, Params, <<"/lib/images/spinner.gif">>),
    {ok, <<"<div id=\"spinner\" class=\"spinner\" style=\"display: none\"><img alt=\"activity indicator\" src=\"">>,Image,<<"\" /></div>">>}.
