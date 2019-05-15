%% @author Marc Worrell
%% @copyright 2014-2019 Marc Worrell
%%
%% @doc Helper functions commonly used in controllers.

%% Copyright 2014-2019 Marc Worrell
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
-include("zotonic.hrl").

-export([
    is_authorized/1,
    is_authorized/2,
    is_authorized/3,
    get_id/1,
    get_configured_id/1,
    decode_request/2,
    encode_response/2
 ]).

% Default max body length (32MB) for HTTP requests, this should be configurable.
-define(MAX_BODY_LENGTH, 32*1024*1024).

% @doc Check if the current user is allowed to access the controller.
-spec is_authorized( z:context() ) -> {boolean(), z:context()}.
is_authorized(Context) ->
    case z_context:get(acl, Context) of
        undefined ->
            is_authorized(append_acl([], Context), Context);
        ignore ->
            {true, Context};
        is_auth ->
            case z_auth:is_auth(Context) of
                true -> is_authorized(append_acl([], Context), Context);
                false -> {false, Context}
            end;
        logoff ->
            Context1 = case z_auth:is_auth(Context) of
               true -> z_auth:logoff(Context);
               false -> Context
            end,
            is_authorized(append_acl([], Context), Context1);
        Acl ->
            is_authorized(append_acl(Acl, Context), Context)
    end.

-spec is_authorized(boolean() | z_acl:acl(), z:context()) -> {boolean(), z:context()}.
is_authorized(true, Context) ->
    {true, Context};
is_authorized(false, Context) ->
    {false, Context};
is_authorized(ACLs, Context) when is_list(ACLs) ->
    is_authorized(is_allowed(ACLs, Context), Context).

-spec is_authorized(z_acl:action(), z_acl:object(), z:context()) -> {boolean(), z:context()}.
is_authorized(Action, Object, Context) ->
    is_authorized([{Action, Object}], Context).


%% Check list of {Action,Object} ACL pairs
-spec is_allowed( z_acl:acl(), z:context() ) -> boolean().
is_allowed([], _Context) ->
    true;
is_allowed([ {Action,Object} | ACLs ], Context) ->
    case z_acl:is_allowed(Action, Object, Context) of
        true ->
            is_allowed(ACLs, Context);
        false ->
            %% If the resource doesn't exist then we let the request through
            %% This will enable a 404 response later in the http flow checks.
            case {Action, Object} of
                {view, undefined} -> is_allowed(ACLs, Context);
                {view, false} -> is_allowed(ACLs, Context);
                {view, Id} ->
                    case m_rsc:exists(Id, Context) of
                        true -> false;
                        false -> is_allowed(ACLs, Context)
                    end;
                _ ->
                    false
            end
    end.

append_acl({_, _} = Acl, Context) ->
    [ get_acl_action(Context), Acl ];
append_acl(Acl, Context) when is_list(Acl) ->
    [ get_acl_action(Context) | Acl ].

get_acl_action(Context) ->
    {z_context:get(acl_action, Context, view), get_id(Context)}.


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


%% @doc Decode the request data
-spec decode_request( undefined | cow_http_hd:media_type(), z:context() ) -> { map() | binary(), z:context() }.
decode_request(undefined, Context) ->
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
decode_request({<<"application">>, <<"x-www-form-urlencoded">>, _}, Context) ->
    from_qs(Context);
decode_request({<<"multipart">>, <<"form-data">>, _}, Context) ->
    from_qs(Context);
decode_request(_CT, Context) ->
    req_body(Context).

%% @doc Decode the incoming body
from_json(Context) ->
    {Body, Context1} = req_body(Context),
    Data = jsxrecord:decode(Body),
    {Data, Context1}.

%% @doc Decode the incoming body
from_qs(Context) ->
    Context1 = z_context:ensure_qs(Context),
    Qs = z_context:get_q_all_noz(Context1),
    {maps:from_list(Qs), Context1}.

-spec req_body( z:context() ) -> binary().
req_body(Context) ->
    cowmachine_req:req_body(?MAX_BODY_LENGTH, Context).

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

%%
%% Helpers
%%

