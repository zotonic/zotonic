%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Get more results for search result

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

-module(action_base_moreresults).
-include("zotonic.hrl").
-export([render_action/4, event/2]).

render_action(TriggerId, TargetId, Args, Context) ->
    Model = proplists:get_value(result, Args),
    Result = Model#m.value,

    SearchName = Result#m_search_result.search_name,
    SearchProps = proplists:delete(page, Result#m_search_result.search_props),

    Page = case proplists:get_value(page, Result#m_search_result.search_props) of
               undefined -> 2;
               P         -> z_convert:to_integer(P)+1
           end,
    PageLen = proplists:get_value(pagelen, SearchProps, 20),

    Template = proplists:get_value(template, Args),
    make_postback(SearchName, SearchProps, Page, PageLen, Template, TriggerId, TargetId, Context).



%% @doc Show more results.
%% @spec event(Event, Context1) -> Context2
event({postback, {moreresults, SearchName, SearchProps, Page, PageLen, Template}, TriggerId, TargetId}, Context) ->
    SearchProps1 = [{page, Page}|SearchProps],
    R = m_search:search({SearchName, SearchProps1}, Context),
    Result = R#m_search_result.result,
    Ids = case proplists:get_value(ids, Result#search_result.result) of
              undefined ->
                  Result#search_result.result;
              X -> X
          end,

    Context1 = case length(Ids) < PageLen of
                   false ->
                       {JS, Ctx} = make_postback(SearchName, SearchProps, Page+1, PageLen, Template, TriggerId, TargetId, Context),
                       RebindJS = ["$(\"#", TriggerId, "\").unbind(\"click\").click(function(){", JS, "});"],
                       z_context:add_script_page(RebindJS, Ctx),
                       Ctx;
                   true ->
                       JS2 = ["$(\"#", TriggerId, "\").remove();"],
                       z_context:add_script_page(JS2, Context),
                       Context
               end,

    lists:foldr(fun (Id, Ctx) ->
                        Html = z_template:render(Template, [{id, Id}], Ctx),
                        z_render:appear_bottom(TargetId, Html, Ctx)
                end, Context1, Ids).


make_postback(SearchName, SearchProps, Page, PageLen, Template, TriggerId, TargetId, Context) ->
    Postback = {moreresults, SearchName, SearchProps, Page, PageLen, Template},
	{PostbackJS, _PickledPostback} = z_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
	{PostbackJS, Context}.
