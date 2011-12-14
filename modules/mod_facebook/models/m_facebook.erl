%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2011 Maas-Maarten Zeeman 
%%
%% @doc Model for accessing facebook data via the graph api.

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

    search/3,
	 
    do_graph_call/5
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(CT, M=#m{value=undefined}, _Context)  
  when CT == friends; 
       CT == home;
       CT == feed;
       CT == likes;
       CT == movies;
       CT == music;
       CT == books;
       CT == notes;
       CT == permissions;
       CT == picture;
       CT == photos;
       CT == albums;
       CT == videos;
       CT == events;
       CT == groups; 
       CT == checkins ->
    M#m{value=CT};
m_find_value(Key, #m{value=picture}, Context) ->
    %% Getting the picture is strangely enough different from all other fields.
    P = do_graph_call(get, Key, undefined, [{fields, "picture"}], Context),
    proplists:get_value(picture, P);
m_find_value(Key, #m{value=ConnectionType}, Context) ->
    do_graph_call(get, Key, ConnectionType, [], Context). 

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> []
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%% @doc Return the search as used by z_search and the search model.
search({fql, Args}, _OfffsetLimit, _Context) ->
    #search_sql{select="dummy", from="dummy", args=Args, run_func=fun facebook_q/4 }.

%% Experimental feature to do facebook fql queries.
facebook_q(_Q, _Sql, Args, Context) ->
    Query = proplists:get_value('query', Args),
    FqlUrl = fql_url(Query, Context),

    %% 
    Payload = case httpc:request(FqlUrl) of
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
		   ?DEBUG(Payload),
		   z_convert:convert_json(Payload)
    end,
  
    #search_result{result=Rows}.

%% @doc Do a facebook graph call. See http://developers.facebook.com/docs/reference/api/ for more info
%%
do_graph_call(Method, Id, Connection, Args, Context) 
  when Method == get; Method == post; Method == delete ->
    ReqArgs = case z_context:get_session(facebook_access_token, Context) of
		  undefined -> Args;
		  AccessToken -> [{access_token, AccessToken} | Args]
	      end,
    Query = mochiweb_util:urlencode(ReqArgs),

    Path = [$/, string:join([z_utils:url_encode(C) || C <- [Id, Connection], C =/= undefined], "/")],

    Request = make_httpc_request(Method, "https", "graph.facebook.com", Path, Query),

    Payload = case httpc:request(Method, Request, [], []) of
		  {ok, {{_, 200, _}, _Headers, Body}} ->
		      mochijson2:decode(Body);
		  Other ->
		      ?DEBUG({error, {http_error, element(1, Request), Other}}),
		      []
	      end,

    z_convert:convert_json(Payload).

%% Create a http request for the inets httpc api.
%%
make_httpc_request(post, Scheme, Server, Path, Query) ->
    Url = mochiweb_util:urlunsplit({Scheme, Server, Path, [], []}),
    {Url, [], "application/x-www-form-urlencoded", Query};
make_httpc_request(Method, Scheme, Server, Path, Query) when Method == get; 
								   Method == delete ->
    Url = mochiweb_util:urlunsplit({Scheme, Server, Path, Query, []}),
    {Url, []}.


%%
%%
fql_url(Query, Context) ->
    Fql = "https://api.facebook.com/method/fql.query?format=json&query=" ++ z_utils:url_encode(Query),
    case z_context:get_session(facebook_access_token, Context) of
        undefined -> Fql;
        AccessToken -> Fql ++ "&access_token=" ++ z_utils:url_encode(AccessToken)
    end.
    
    



		  
					   
    
    
