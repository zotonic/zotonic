%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Create a button for opening a dialog where the user can select an object for a new edge.

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

-module(scomp_admin_dialog_link_add).
-behaviour(gen_scomp).

-export([init/1, varies/2, terminate/2, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
terminate(_State, _Context) -> ok.

render(Params, _Vars, Context, _State) ->
    Id        = z_ids:optid(proplists:get_value(id, Params)),
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Params)), 
    Predicate = proplists:get_value(predicate, Params), 
    ElementId = proplists:get_value(element_id, Params),
    Anchor = z_tags:render_tag(
                        <<"a">>,
                        [
                    		{<<"id">>,    Id},
                    		{<<"href">>,  <<"javascript:void(0)">>},
                    		{<<"title">>, "add a connection"},
                    		{<<"class">>, "link-add"}
                    	],
                    	"+ add a connection"),

    Html = [<<"<span class=\"add-connection\">">>, Anchor, <<"</span>">>],
    Context1 = z_render:render(Html, Context),
    Context2 = z_render:wire(
                Id, 
                {event,[
                        {type, click}, 
                        {action, {dialog_link, [{subject_id, SubjectId}, {predicate, Predicate}, {element_id, ElementId}]} }
                ]}, Context1),
    {ok, Context2}.
