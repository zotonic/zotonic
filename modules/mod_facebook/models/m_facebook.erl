%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2011 Maas-Maarten Zeeman 
%% Date: 2011-08-15
%%
%% @doc Model for managing the comments on a page.

%% Copyright 2011 Maas-Maarten Zeeman
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

-module(m_facebook).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    search/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(_Key, #m{value=undefined}, _Context) ->
   %% Do a graph query. 
   undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> []
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%% @doc Return the search as used by z_search and the search model.
search({fql, []}, _OfffsetLimit, _Context) ->
    %% TODO ...  
    #search_sql{select="first_name", from="user", where="uid=me()", run_func=fun facebook_q/4 }.

%% Experimental feature to do facebook fql queries.
facebook_q(Q, Sql, Args, Context) ->
    ?DEBUG({facebook_q, Q, Sql, Args}),

    %% TODO Construct a valid FQL Query from the arguments
    Query = "select uid, first_name, last_name, online_presence from user where uid in (select uid2 from friend where uid1 = me())",
    FqlUrl = fql_url(Query, Context),

    %% 
    Payload = case http:request(FqlUrl) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            mochijson2:decode(Body);
        Other ->
            ?DEBUG({error, {http_error, FqlUrl, Other}}),
            []
    end,

    %%
    Rows = case Payload of
       {struct, [{<<"error_code">>, _ErrorCode} | _T]} ->
           ?DEBUG(Payload),
           [];
       _ ->
        [ [{z_convert:to_atom(K), V} || {K, V} <- PropList] || {struct, PropList} <- Payload]
    end,
  
    #search_result{result=Rows}.


fql_url(Query, Context) ->
    Fql = "https://api.facebook.com/method/fql.query?format=json&query=" ++ z_utils:url_encode(Query),
    case z_context:get_session(facebook_access_token, Context) of
        undefined -> Fql;
        AccessToken -> Fql ++ "&access_token=" ++ z_utils:url_encode(AccessToken)
    end.
