%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-04
%% @doc Return a list of ids of a category, sorted by title.  This needs to fetch and sort all 
%% resources of that category, so use this with care for situations where you know that the number
%% of returned resources is relatively low.

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

-module(search_all_bytitle).
-author("Marc Worrell <marc@worrell.nl>").

%% interface functions
-export([
    search/3,
	search_cat_is/3
]).

-include_lib("zotonic.hrl").

search([Cat], Search, Context) ->
    search(Cat, Search, Context);
search(Cat, Search, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            {Left,Right} = m_category:get_range(CatId, Context),
			search1(Left, Right, Search, Context);
        {error, _Reason} ->
            #search_result{}
    end.

search_cat_is(Cat, Search, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
			Props = m_category:get(CatId, Context),
			Nr = proplists:get_value(nr, Props),
			search1(Nr, Nr, Search, Context);
        {error, _Reason} ->
            #search_result{}
    end.

search1(Left, Right, Search, Context) ->
	Ids = z_db:q("select id, is_featured from rsc where pivot_category_nr >= $1 and pivot_category_nr <= $2", [Left,Right], Context),
    IdTitles = [ add_title(Id, Featured, Search, Context) || {Id, Featured} <- Ids, m_rsc:is_visible(Id, Context) ],
    Sorted = lists:sort(IdTitles),
    Result = [ {Title, Id} || {_Name, Title, Id} <- Sorted ],
    #search_result{result=Result, all=Sorted, total=length(Sorted)}.


    add_title(Id, true, all_bytitle_featured, Context) ->
        Title = ?__(m_rsc:p(Id, title, Context), Context),
        {[32 | z_string:to_name(Title)], Title, Id};
    add_title(Id, _Featured, _Search, Context) ->
        Title = ?__(m_rsc:p(Id, title, Context), Context),
        {z_string:to_name(Title), Title, Id}.
