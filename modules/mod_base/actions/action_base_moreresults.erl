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
    Result = case proplists:get_value(result, Args) of
                #m{model=m_search, value=V} -> V;
                #m_search_result{} = M -> M
            end,

    SearchName = Result#m_search_result.search_name,
    SearchProps = proplists:delete(page, Result#m_search_result.search_props),
    SearchResult = Result#m_search_result.result,
    
    Page = case z_convert:to_integer(proplists:get_value(page, Result#m_search_result.search_props, 1)) of
               undefined -> 2;
               P -> P+1
           end,
    PageLen = z_convert:to_integer(proplists:get_value(pagelen, SearchProps, 20)),
    MorePageLen = proplists:get_value(pagelen, Args, PageLen),

    ResultLen = case proplists:get_value(ids, SearchResult#search_result.result) of
                    undefined -> length(SearchResult#search_result.result);
                    Ids -> length(Ids)
                end,
    case ResultLen < PageLen of
        true ->
            {"", z_script:add_script(["$(\"#", TriggerId, "\").remove();"], Context)};
        false ->
            make_postback(SearchName, SearchProps, Page, PageLen, MorePageLen, Args, TriggerId, TargetId, Context)
    end.




%% @doc Show more results.
%% @spec event(Event, Context1) -> Context2
%% @todo Handle the "MorePageLen" argument correctly.
event(#postback{message={moreresults, SearchName, SearchProps, Page, PageLen, MorePageLen, Args}, trigger=TriggerId, target=TargetId}, Context) ->
    SearchProps1 = [{page, Page}|SearchProps],
    R = m_search:search({SearchName, SearchProps1}, Context),
    Result = R#m_search_result.result,
    Ids = case proplists:get_value(ids, Result#search_result.result) of
              undefined -> Result#search_result.result;
              X -> X
          end,

    Context1 = case length(Ids) < PageLen of
                   false ->
                        {JS, Ctx} = make_postback(SearchName, SearchProps, Page+1, PageLen, MorePageLen, Args, TriggerId, TargetId, Context),
                        RebindJS = case proplists:get_value(visible, Args) of
                           true ->
                               [ <<"z_on_visible('#">>, TriggerId, <<"', function() {">>, JS, <<"});\n">> ];
                           _ ->
                               ["$(\"#", TriggerId, "\").unbind(\"click\").click(function(){", JS, "; return false; });"]
                        end,
                        z_script:add_script(RebindJS, Ctx);
                   true ->
                        RemoveJS = ["$(\"#", TriggerId, "\").remove();"],
                        z_script:add_script(RemoveJS, Context)
               end,

    FirstRow = PageLen*(Page-1)+1,
    Ids1 = lists:zip3(lists:map(fun to_id/1, Ids), Ids, lists:seq(FirstRow, FirstRow+length(Ids)-1)),
    Html = lists:map(fun({Id, ResultRow, RowNr}) -> 
                        Vars = [
                                {id, Id}, {result_row, ResultRow}, {row, RowNr}, {is_first, RowNr == FirstRow}
                               ] ++ Args,
                             Template = proplists:get_value(template, Args),
                             case proplists:get_value(catinclude, Args) of
                                 true -> z_template:render({cat, Template}, Vars, Context1);
                                 _ -> z_template:render(Template, Vars, Context1)
                             end
                    end, Ids1),
    z_render:appear_bottom(TargetId, Html, Context1).


    to_id(Id) when is_integer(Id) -> Id;
    to_id({Id,_}) when is_integer(Id) -> Id;
    to_id({_,Id}) when is_integer(Id) -> Id;
    to_id(T) when is_tuple(T) -> element(1, T);
    to_id([{_,_}|_] = L) -> proplists:get_value(id, L).

make_postback(SearchName, SearchProps, Page, PageLen, MorePageLen, Args, TriggerId, TargetId, Context) ->
    Postback = {moreresults, SearchName, SearchProps, Page, PageLen, MorePageLen, Args},
    {PostbackJS, _PickledPostback} = z_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
    {PostbackJS, Context}.
