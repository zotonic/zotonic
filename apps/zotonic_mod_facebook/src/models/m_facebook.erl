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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    search/3,
    is_useauth/1,

    do_graph_call/5
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ useauth | Rest ], _OptMsg, Context) ->
    {ok, {is_useauth(Context), Rest}};
m_get([ picture, Key | Rest ], _OptMsg, Context) ->
    P = do_graph_call(get, Key, undefined, [{fields, "picture"}], Context),
    {ok, {proplists:get_value(picture, P), Rest}};
m_get([ CT, Key | Rest ], _OptMsg, Context)
  when CT =:= friends;
       CT =:= home;
       CT =:= feed;
       CT =:= likes;
       CT =:= movies;
       CT =:= music;
       CT =:= books;
       CT =:= notes;
       CT =:= permissions;
       CT =:= photos;
       CT =:= albums;
       CT =:= videos;
       CT =:= events;
       CT =:= groups;
       CT =:= checkins ->
    {ok, {do_graph_call(get, Key, CT, [], Context), Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


-spec is_useauth( z:context() ) -> boolean().
is_useauth(Context) ->
    case m_config:get_value(mod_facebook, appid, Context) of
        undefined -> false;
        <<>> -> false;
        _ -> m_config:get_boolean(mod_facebook, useauth, Context)
    end.

%% @doc Return the search as used by z_search and the search model.
search({fql, Args}, _OfffsetLimit, _Context) ->
    #search_sql{select="dummy", from="dummy", args=Args, run_func=fun facebook_q/4 }.

% %% Experimental feature to do facebook fql queries.
facebook_q(_Q, _Sql, Args, Context) ->
    Query = proplists:get_value('query', Args),
    FqlUrl = fql_url(Query, Context),

    %%
    Payload = case httpc:request(FqlUrl) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            z_json:decode(Body);
        Other ->
            ?DEBUG({error, {http_error, FqlUrl, Other}}),
            #{}
    end,
    Rows = case Payload of
        #{<<"error_code">> := _ErrorCode} ->
            ?DEBUG(Payload),
            #{};
        _ ->
            ?DEBUG(Payload)
    end,
    #search_result{result=Rows}.

%% @doc Do a facebook graph call. See http://developers.facebook.com/docs/reference/api/ for more info
%%
do_graph_call(Method, Id, Connection, Args, Context) when Method == get; Method == post; Method == delete ->
    case z_acl:is_allowed(use, mod_facebook, Context) of
        true ->
            ReqArgs = case m_config:get_value(mod_facebook, facebook_access_token, Context) of
                undefined -> Args;
                <<>> -> Args;
                AccessToken -> [{access_token, AccessToken} | Args]
            end,
            Query = mochiweb_util:urlencode(ReqArgs),
            Path = [$/, string:join([z_url:url_encode(C) || C <- [Id, Connection], C =/= undefined], "/")],
            Request = make_httpc_request(Method, "https", "graph.facebook.com", Path, Query),
            case httpc:request(Method, Request, [], []) of
                {ok, {{_, 200, _}, _Headers, Body}} ->
                    {ok, z_json:decode(Body)};
                Other ->
                    {error, {http_error, element(1, Request), Other}}
            end;
        false ->
            {error, eacces}
    end.

%% Create a http request for the inets httpc api.
%%
make_httpc_request(post, Scheme, Server, Path, Query) ->
    Url = mochiweb_util:urlunsplit({Scheme, Server, Path, [], []}),
    {Url, [], "application/x-www-form-urlencoded", Query};
make_httpc_request(Method, Scheme, Server, Path, Query) when Method == get; Method == delete ->
    Url = mochiweb_util:urlunsplit({Scheme, Server, Path, Query, []}),
    {Url, []}.

%%
%%
fql_url(Query, Context) ->
    Fql = "https://api.facebook.com/method/fql.query?format=json&query=" ++ z_url:url_encode(Query),
    case m_config:get_value(mod_facebook, facebook_access_token, Context) of
        undefined -> Fql;
        <<>> -> Fql;
        AccessToken -> Fql ++ "&access_token=" ++ z_url:url_encode(AccessToken)
    end.
