%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc REST resource for the m.rsc model

%% Copyright 2012 Marc Worrell
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

-module(controller_rest_rsc).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    allowed_methods/1,
    forbidden/1,
    resource_exists/1,
    content_types_provided/1,
    content_types_accepted/1,
    charsets_provided/1,
    delete_resource/1,
    put_json/1,
    get_json/1,
    put_bert/1,
    get_bert/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context2 = z_context:set_noindex_header( z_context:ensure_qs(Context) ),
    z_context:lager_md(Context2),
    {true, Context2}.

allowed_methods(Context) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Context}.


forbidden(Context) ->
    case z_acl:is_allowed(use, mod_rest, Context) of
        true ->
            IsAuthorized = case get_id(Context) of
                               {ok, Id} ->
                                   case m_rsc:exists(Id, Context) of
                                       true ->
                                            case cowmachine_req:method(Context) of
                                                <<"GET">> -> z_acl:rsc_visible(Id, Context);
                                                <<"DELETE">> -> z_acl:rsc_deletable(Id, Context);
                                                <<"PUT">> -> z_acl:rsc_editable(Id, Context)
                                            end;
                                        false ->
                                            true
                                    end;
                                _ ->
                                    true
                           end,
            case IsAuthorized of
                false -> {true, Context};
                true -> {false, Context}
            end;
        false ->
            {true, Context}
    end.


resource_exists(Context) ->
    Exists = case get_id(Context) of
                {ok, Id} -> m_rsc:exists(Id, Context);
                _ -> false
             end,
    {Exists, Context}.


content_types_provided(Context) ->
    Provided = case z_context:get_q(<<"format">>, Context) of
        <<"bert">> -> [ {<<"application/x-bert">>, get_bert} ];
        <<"json">> -> [ {<<"application/json">>, get_json} ];
        _ ->
            [
                {<<"application/json">>, get_json},
                {<<"application/x-bert">>, get_bert}
            ]
    end,
    {Provided, Context}.

content_types_accepted(Context) ->
    Accepted = case z_context:get_q(<<"format">>, Context) of
        <<"bert">> -> [ {<<"application/x-bert">>, put_bert} ];
        <<"json">> -> [ {<<"application/json">>, put_json} ];
        _ ->
            [
                {<<"application/json">>, put_json},
                {<<"application/x-bert">>, put_bert}
            ]
    end,
    {Accepted, Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.


delete_resource(Context) ->
    {ok, Id} = get_id(Context),
    try m_rsc:delete(Id, Context) of
        {ok, _} -> {true, Context}
    catch
        {error, _} -> {false, Context}
    end.


get_json(Context) ->
    {ok, Id} = get_id(Context),
    Data = z_json:encode(get_rsc(Id, Context)),
    do_get(Id, Data, <<".json">>, Context).

get_bert(Context) ->
    {ok, Id} = get_id(Context),
    Data = bert:encode(get_rsc(Id, Context)),
    do_get(Id, Data, <<".bert">>, Context).

do_get(Id, Data, Extension, Context) ->
    Modified = m_rsc:p(Id, modified, Context),
    Disp = iolist_to_binary([
                    "attachment; filename=", z_context:hostname(Context),
                    $-,
                    integer_to_list(Id),
                    $-,
                    m_rsc:p(Id, slug, Context),
                    $-,
                    z_datetime:format(Modified, "YmdHis", Context),
                    Extension
                ]),
    Context1 = z_context:set_resp_header(<<"content-disposition">>, Disp, Context),
    {Data, Context1}.


%% @doc Fetch the request body, translate to properties and hand to the update routines.
%% @todo Handle update errors
put_json(Context) ->
    {Body, Context1} = cowmachine_req:req_body(Context),
    Parsed = z_json:decode(Body),
    do_update(Parsed, Context1).

put_bert(Context) ->
    {Body, Context1} = cowmachine_req:req_body(Context),
    Parsed = bert:decode(Body),
    do_update(Parsed, Context1).


do_update(Props, Context) ->
    {ok, Id} = get_id(Context),
    case catch mod_rest:rsc_upload(Id, Props, Context) of
        {ok, _} ->
            {true, Context};
        {error, _Reason} ->
            {true, Context}
    end.


%% @doc Fetch the resource id from the request
get_id(Context) ->
    case erlang:get(rsc_id) of
        undefined ->
            IdArg = z_context:get_q(<<"id">>, Context),
            Result = case m_rsc:rid(IdArg, Context) of
                        undefined -> {error, not_found};
                        Id -> {ok, Id}
                     end,
            erlang:put(rsc_id, Result),
            Result;
        Result ->
            Result
    end.


%% @doc Make sure that we do an ACL check for all props before pushing them out.
%% @todo Move this to the m_rsc module
get_rsc(Id, Context) ->
    Props = m_rsc:get_visible(Id, Context),
    IsA = m_rsc:is_a(Id, Context),
    Complete = [
        {category, hd(IsA)},
        {content_group, content_group(Id, Context)},
        {page_url, z_convert:to_binary(z_context:abs_url(m_rsc:p(Id, page_url, Context), Context))},
        {computed_category, IsA},
        {computed_site_name, z_context:site(Context)},
        {computed_address_country, m_l10n:country_name(proplists:get_value(address_country, Props), Context)},
        {computed_mail_country, m_l10n:country_name(proplists:get_value(mail_country, Props), Context)},
        {medium, medium(Id, Context)}
        | Props
    ],
    lists:filter(
        fun({_, ?ST_JUTTEMIS}) -> false;
           ({computed_address_country, <<>>}) -> false;
           ({computed_mail_country, <<>>}) -> false;
           ({category_id, _}) -> false;
           ({content_group_id, _}) -> false;
           (_) -> true
        end,
        Complete).

medium(Id, Context) ->
    case m_media:get(Id, Context) of
        undefined ->
            undefined;
        M ->
            [
                {medium_url, z_convert:to_binary(
                                z_context:abs_url(
                                    z_dispatcher:url_for(media_attachment, [{id,Id}], Context),
                                    Context))}
                | M
            ]
    end.

content_group(Id, Context) ->
    CGId = m_rsc:p_no_acl(Id, content_group_id, Context),
    case m_rsc:p_no_acl(CGId, name, Context) of
        undefined -> CGId;
        Name -> Name
    end.

