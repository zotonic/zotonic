%% @author Marc Worrell
%% @copyright 2014-2022 Marc Worrell
%%
%% @doc Helper functions commonly used in controllers.

%% Copyright 2014-2022 Marc Worrell
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

-module(z_controller_helper).

-export([
    is_authorized/2,
    is_authorized/3,
    is_authorized_action/3,
    get_id/1,
    get_configured_id/1,
    decode_request/2,
    decode_request_noz/2,
    encode_response/2,
    req_body/1
 ]).

% Default max body length (32MB) for HTTP requests, this should be configurable.
-define(MAX_BODY_LENGTH, 32*1024*1024).

% @doc Check if the current user is allowed to access the controller.
-spec is_authorized
    (OptRscId, Context) -> {boolean(), Context1}
        when OptRscId :: m_rsc:resource_id() | undefined,
             Context :: z:context(),
             Context1 :: z:context();
    (ACLs, Context) -> {boolean(), Context1}
        when ACLs :: [ z_acl:acl() ],
             Context :: z:context(),
             Context1 :: z:context().
is_authorized(ACLs, Context) when is_list(ACLs) ->
    is_authorized(undefined, ACLs, Context);
is_authorized(OptRscId, Context) ->
    case z_context:get(acl, Context) of
        undefined ->
            is_authorized(OptRscId, append_acl(OptRscId, [], Context), Context);
        ignore ->
            {true, Context};
        is_auth ->
            case z_auth:is_auth(Context) of
                true -> is_authorized(OptRscId, append_acl(OptRscId, [], Context), Context);
                false -> {false, Context}
            end;
        logoff ->
            Context1 = case z_auth:is_auth(Context) of
               true -> z_auth:logoff(Context);
               false -> Context
            end,
            is_authorized(OptRscId, append_acl(OptRscId, [], Context), Context1);
        Acl ->
            is_authorized(OptRscId, append_acl(OptRscId, Acl, Context), Context)
    end.

-spec is_authorized(OptRscId, ACL, z:context()) -> {boolean(), z:context()}
    when OptRscId :: m_rsc:resource_id() | undefined,
         ACL :: boolean() | z_acl:acl().
is_authorized(_OptRscId, true, Context) ->
    {true, Context};
is_authorized(_OptRscId, false, Context) ->
    {false, Context};
is_authorized(OptRscId, ACLs, Context) when is_list(ACLs) ->
    {is_allowed(OptRscId, ACLs, Context), Context}.

-spec is_authorized_action(z_acl:action(), z_acl:object(), z:context()) -> {boolean(), z:context()}.
is_authorized_action(Action, Object, Context) ->
    is_authorized(undefined, [{Action, Object}], Context).

%% Check list of {Action,Object} ACL pairs
-spec is_allowed(OptRscId, z_acl:acl(), z:context() ) -> boolean()
    when OptRscId :: m_rsc:resource_id() | undefined.
is_allowed(_OptRscId, [], _Context) ->
    true;
is_allowed(OptRscId, [ {Action,Object} | ACLs ], Context) ->
    case z_acl:is_allowed(Action, Object, Context) of
        true ->
            is_allowed(OptRscId, ACLs, Context);
        false ->
            %% If the resource doesn't exist then we let the request through
            %% This will enable a 404 response later in the http flow checks.
            case {Action, Object} of
                {view, undefined} -> is_allowed(OptRscId, ACLs, Context);
                {view, false} -> is_allowed(OptRscId, ACLs, Context);
                {view, Id} ->
                    case m_rsc:exists(Id, Context) of
                        true -> false;
                        false -> is_allowed(OptRscId, ACLs, Context)
                    end;
                _ ->
                    false
            end
    end.

append_acl(OptRscId, {_, _} = Acl, Context) ->
    [ get_acl_action(OptRscId, Context), Acl ];
append_acl(OptRscId, Acl, Context) when is_list(Acl) ->
    [ get_acl_action(OptRscId, Context) | Acl ].

get_acl_action(OptRscId, Context) ->
    {z_context:get(acl_action, Context, view), OptRscId}.


%% @doc Fetch the id from the request or the dispatch configuration.
-spec get_id(z:context()) -> m_rsc:resource_id() | undefined.
get_id(Context) ->
    case get_configured_id(Context) of
        undefined -> m_rsc:rid(z_context:get_q(<<"id">>, Context), Context);
        ConfId -> ConfId
    end.

%% @doc Fetch the id from the dispatch configuration.
-spec get_configured_id(z:context()) -> m_rsc:resource_id() | undefined.
get_configured_id(Context) ->
    case z_context:get(id, Context) of
        user_id -> z_acl:user(Context);
        ConfId -> m_rsc:rid(ConfId, Context)
    end.


%% @doc Decode the request data - remove zotonic arguments that are part
%% of the query string and/or post.
-spec decode_request_noz( undefined | cow_http_hd:media_type(), z:context() ) -> { map() | binary(), z:context() }.
decode_request_noz(undefined, Context) ->
    from_qs_noz(Context);
decode_request_noz({<<"application">>, <<"x-www-form-urlencoded">>, _}, Context) ->
    from_qs_noz(Context);
decode_request_noz({<<"multipart">>, <<"form-data">>, _}, Context) ->
    from_qs_noz(Context);
decode_request_noz({<<"application">>, <<"json">>, _}, Context) ->
    from_json(Context);
decode_request_noz({<<"application">>, <<"javascript">>, _}, Context) ->
    from_json(Context);
decode_request_noz({<<"text">>, <<"javascript">>, _}, Context) ->
    from_json(Context);
decode_request_noz({<<"text">>, <<"x-ubf">>, _} = Mime, Context) ->
    decode_request(Mime, Context);
decode_request_noz({<<"application">>, <<"x-bert">>, _} = Mime, Context) ->
    decode_request(Mime, Context);
decode_request_noz(_CT, Context) ->
    case cowmachine_req:method(Context) of
        <<"GET">> -> from_qs_noz(Context);
        <<"DELETE">> -> from_qs_noz(Context);
        _ -> req_body(Context)
    end.

%% @doc Decode the request data
-spec decode_request( undefined | cow_http_hd:media_type(), z:context() ) -> { map() | binary(), z:context() }.
decode_request(undefined, Context) ->
    from_qs(Context);
decode_request({<<"application">>, <<"x-www-form-urlencoded">>, _}, Context) ->
    from_qs(Context);
decode_request({<<"multipart">>, <<"form-data">>, _}, Context) ->
    from_qs(Context);
decode_request({<<"application">>, <<"json">>, _}, Context) ->
    from_json(Context);
decode_request({<<"application">>, <<"javascript">>, _}, Context) ->
    from_json(Context);
decode_request({<<"text">>, <<"javascript">>, _}, Context) ->
    from_json(Context);
decode_request({<<"text">>, <<"x-ubf">>, _}, Context) ->
    {Body, Context1} = req_body(Context),
    {Data, _Rest} = z_ubf:decode(Body),
    {Data, Context1};
decode_request({<<"application">>, <<"x-bert">>, _}, Context) ->
    {Body, Context1} = req_body(Context),
    Data = erlang:binary_to_term(Body, [safe]),
    {Data, Context1};
decode_request(_CT, Context) ->
    case cowmachine_req:method(Context) of
        <<"GET">> -> from_qs(Context);
        <<"DELETE">> -> from_qs(Context);
        _ -> req_body(Context)
    end.

%% @doc Decode the incoming body
from_json(Context) ->
    {Body, Context1} = req_body(Context),
    Data = jsxrecord:decode(Body),
    {Data, Context1}.

%% @doc Make a map from the query arguments.
from_qs(Context) ->
    Context1 = z_context:ensure_qs(Context),
    {z_context:get_q_map(Context1), Context1}.

%% @doc Make a map from the query arguments.
from_qs_noz(Context) ->
    Context1 = z_context:ensure_qs(Context),
    {z_context:get_q_map_noz(Context1), Context1}.

-spec req_body( z:context() ) -> {binary(), z:context()}.
req_body(Context) ->
    case cowmachine_req:req_body(?MAX_BODY_LENGTH, Context) of
        {undefined, Context1} -> from_qs(Context1);
        {Body, Context1} -> {Body, Context1}
    end.

%% @doc Encode the response data
-spec encode_response( Mime :: cow_http_hd:media_type(), term() ) -> binary().
encode_response({<<"application">>, <<"json">>, _}, Data) ->
    jsxrecord:encode(Data);
encode_response({<<"application">>, <<"javascript">>, _}, Data) ->
    jsxrecord:encode(Data);
encode_response({<<"text">>, <<"javascript">>, _}, Data) ->
    jsxrecord:encode(Data);
encode_response({<<"text">>, <<"x-ubf">>, _}, Data) ->
    {ok, UBF} = z_ubf:encode(Data),
    UBF;
encode_response({<<"application">>, <<"x-bert">>, _}, Data) ->
    erlang:term_to_binary(Data);
encode_response({<<"application">>, <<"x-www-form-urlencoded">>, _}, Data) ->
    cow_qs:urlencode( encode_prep_qs(Data) ).

encode_prep_qs(Map) when is_map(Map) ->
    encode_prep_qs( maps:to_list(Map) );
encode_prep_qs(List) when is_list(List) ->
    lists:map(
        fun
            ({K, undefined}) -> {z_convert:to_binary(K), <<>>};
            ({K, true}) -> {z_convert:to_binary(K), true};
            ({K, false}) -> {z_convert:to_binary(K), false};
            ({K, V}) -> {z_convert:to_binary(K), z_convert:to_binary(V)};
            (K) when is_atom(K) -> {atom_to_binary(K, utf8), true};
            (K) when is_binary(K) -> {K, true}
        end,
        List).

