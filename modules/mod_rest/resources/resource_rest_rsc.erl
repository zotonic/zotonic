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

-module(resource_rest_rsc).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    is_authorized/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2,
    delete_resource/2,
    put_json/2,
    get_json/2,
    put_bert/2,
    get_bert/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_qs(z_context:set(DispatchArgs, Context)),
    Context2 = z_context:set_noindex_header(z_context:continue_session(Context1)),
    ?WM_REPLY(true, Context2).


allowed_methods(ReqData, Context) ->
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.


is_authorized(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    IsAuthorized = case get_id(Context) of
                       {ok, Id} ->
                           case m_rsc:exists(Id, Context) of
                               true ->
                                    case webmachine_request:method(ReqData) of
                                        'GET' -> z_acl:rsc_visible(Id, Context);
                                        'DELETE' -> z_acl:rsc_deleteable(Id, Context);
                                        'PUT' -> z_acl:rsc_editable(Id, Context)
                                    end;
                                false ->
                                    true
                            end;
                        _ ->
                            true
                   end,
    case IsAuthorized of
        false -> ?WM_REPLY("Zotonic-Auth", Context);
        true -> ?WM_REPLY(true, Context)
    end.


resource_exists(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Exists = case get_id(Context) of
                {ok, Id} -> m_rsc:exists(Id, Context);
                _ -> false
             end,
    ?WM_REPLY(Exists, Context).


content_types_provided(ReqData, Context) ->
    Provided = case z_context:get_q("format", Context) of
        "bert" -> [ {"application/x-bert", get_bert} ];
        "json" -> [ {"application/json", get_json} ];
        _ -> [ {"application/json", get_json}, {"application/x-bert", get_bert} ]
    end,
    {Provided, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    Accepted = case z_context:get_q("format", Context) of
        "bert" -> [ {"application/x-bert", put_bert} ];
        "json" -> [ {"application/json", put_json} ];
        _ -> [ {"application/json", put_json}, {"application/x-bert", put_bert} ]
    end,
    {Accepted, ReqData, Context}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.


delete_resource(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {ok, Id} = get_id(Context),
    case m_rsc:delete(Id, Context) of
        {ok, _} -> ?WM_REPLY(true, Context);
        {error, _} -> ?WM_REPLY(false, Context)
    end.


get_json(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {ok, Id} = get_id(Context),
    Data = mochijson:binary_encode(z_json:to_mochijson(get_rsc(Id, Context), Context)),
    do_get(Id, Data, ".json", Context).

get_bert(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    {ok, Id} = get_id(Context),
    Data = bert:encode(get_rsc(Id, Context)),
    do_get(Id, Data, ".bert", Context).

do_get(Id, Data, Extension, Context) ->
    Modified = m_rsc:p(Id, modified, Context),
    Disp = iolist_to_binary([
                    "attachment; filename=", z_context:hostname(Context),
                    $-,   
                    integer_to_list(Id),
                    $-, 
                    erlydtl_dateformat:format(Modified, "YmdHis", Context),
                    Extension
                ]),
    Context1 = z_context:set_resp_header("Content-Disposition", z_convert:to_list(Disp), Context),
    ?WM_REPLY(Data, Context1).


%% @doc Fetch the request body, translate to properties and hand to the update routines.
%% @todo Handle update errors
put_json(ReqData, Context0) ->
    {Body, ReqData1} = wrq:req_body(ReqData),
    Context = ?WM_REQ(ReqData1, Context0),
    Parsed = z_json:from_mochijson(mochijson:binary_decode(Body), Context),
    do_update(Parsed, Context).

put_bert(ReqData, Context0) ->
    {Body, ReqData1} = wrq:req_body(ReqData),
    Context = ?WM_REQ(ReqData1, Context0),
    Parsed = bert:decode(Body),
    do_update(Parsed, Context).


do_update(Props, Context) ->
    {ok, Id} = get_id(Context),
    case catch mod_rest:rsc_upload(Id, Props, Context) of
        {ok, _} ->
            ?WM_REPLY(true, Context);
        {error, _Reason} ->
            ?WM_REPLY(true, Context)
    end.


%% @doc Fetch the resource id from the request
get_id(Context) ->
    case erlang:get(rsc_id) of
        undefined ->
            IdArg = wrq:path_info(id, z_context:get_reqdata(Context)),
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
    Props = m_rsc:get(Id, Context),
    Filtered = lists:filter(fun({K,_}) -> z_acl:rsc_prop_visible(Id, K, Context) end, Props),
    Complete = [ 
        {page_url, z_convert:to_binary(z_context:abs_url(m_rsc:p(Id, page_url, Context), Context))},
        {computed_site_name, z_context:site(Context)},
        {computed_address_country, m_l10n:country_name(proplists:get_value(address_country, Props), Context)},
        {computed_mail_country, m_l10n:country_name(proplists:get_value(mail_country, Props), Context)},
        {medium, medium(Id, Context)}
        | Filtered
    ],
    lists:filter(
        fun({_, ?ST_JUTTEMIS}) -> false;
           ({computed_address_country, <<>>}) -> false;
           ({computed_mail_country, <<>>}) -> false;
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

