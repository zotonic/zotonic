%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2014 Marc Worrell
%%
%% @doc Serve a file (possibly resized)

%% Copyright 2013-2014 Marc Worrell
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

%% Serves files like:
%%
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/<filepath>
%% /media/attachment/<filepath>

-module(controller_file).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    resource_exists/2,
    forbidden/2,
    last_modified/2,
    expires/2,
    content_types_provided/2,
    charsets_provided/2,
    content_encodings_provided/2,
    provide_content/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

-define(MAX_AGE, 31536000).


init(ConfigProps) ->
    {ok, ConfigProps}.

%% @doc Initialize the context for the request. Optionally continue the user's session.
service_available(ReqData, ConfigProps) ->
    Context = z_context:new_request(ReqData, ConfigProps, ?MODULE),
    Context1 = z_context:continue_session(z_context:ensure_qs(Context)),
    Context2 = z_context:set_cors_headers([{"Access-Control-Allow-Origin", "*"}], Context1),
    z_context:lager_md(Context2),
    case get_file_info(ConfigProps, Context2) of
        {ok, Info} ->
            IsNoIndex = is_noindex(Info, Context),
            Context3 = z_context:set_noindex_header(IsNoIndex, Context2),
            ReqData1 = z_context:get_reqdata(Context3),
            {true, ReqData1, {Info, Context3}};
        {error, enoent} = Error ->
            Context3 = z_context:set_noindex_header(Context2),
            ReqData1 = z_context:get_reqdata(Context3),
            {true, ReqData1, {Error, Context3}};
        {error, _} = Error ->
            Context3 = z_context:set_noindex_header(Context2),
            ReqData1 = z_context:get_reqdata(Context3),
            {false, ReqData1, {Error, Context3}}
    end.

allowed_methods(ReqData, State) ->
    {['HEAD', 'GET'], ReqData, State}.

resource_exists(ReqData, {{error,enoent},_Context} = State) ->
    {false, ReqData, State};
resource_exists(ReqData, {#z_file_info{acls=Acls}, Context} = State) ->
    {not lists:any(fun(Id) when is_integer(Id) ->
                            not m_rsc:exists(Id, Context);
                      ({module, _Module}) ->
                            false
                   end,
                   Acls), ReqData, State}.

forbidden(ReqData, {{error,_},_Context} = State) ->
    {false, ReqData, State};
forbidden(ReqData, {#z_file_info{} = FInfo,Context}) ->
    Context1 = ?WM_REQ(ReqData, Context),
    case z_controller_helper:is_authorized(Context1) of
        {false, RD1, Context2} ->
            {true, RD1, {FInfo, Context2}};
        {true, RD1, Context2} ->
            {not z_file_request:is_visible(FInfo, Context2), RD1, {FInfo,Context2}}
    end.

last_modified(ReqData, {{error, _},_} = State) ->
    {calendar:universal_time(), ReqData, State};
last_modified(ReqData, {#z_file_info{modifiedUTC=LModUTC},_Context} = State) ->
    {LModUTC, ReqData, State}.

expires(ReqData, {_Info,Context} = State) ->
    Date = calendar:universal_time(),
    case z_context:get(max_age, Context, ?MAX_AGE) of
        N when N >= ?MAX_AGE ->
            {z_datetime:next_year(Date), ReqData, State};
        MaxAge ->
            NowSecs = calendar:datetime_to_gregorian_seconds(Date),
            {calendar:gregorian_seconds_to_datetime(NowSecs + MaxAge), ReqData, State}
    end.

charsets_provided(ReqData, {{error, _},_Context} = State) ->
    {no_charset, ReqData, State};
charsets_provided(ReqData, {#z_file_info{mime=Mime},_Context} = State) ->
    case is_text(Mime) of
        true -> {["utf-8"], ReqData, State};
        _ -> {no_charset, ReqData, State}
    end.

content_encodings_provided(ReqData, {{error, _},_Context} = State) ->
    {["identity"], ReqData, State};
content_encodings_provided(ReqData, {Info,_Context} = State) ->
    Encs = z_file_request:content_encodings(Info),
    {[z_convert:to_list(Enc)||Enc<-Encs], ReqData, State}.

content_types_provided(ReqData, {{error, _},_Context} = State) ->
    {[{"text/plain", provide_content}], ReqData, State};
content_types_provided(ReqData, {#z_file_info{mime=Mime},_Context} = State) ->
    {[{z_convert:to_list(Mime), provide_content}], ReqData, State}.


provide_content(ReqData, {{error, _}, _} = State) ->
    {<<>>, ReqData, State};
provide_content(ReqData,  {Info,Context} = State) ->
    RD1 = set_content_dispostion(z_context:get(content_disposition, Context), ReqData),
    MaxAge = z_context:get(max_age, Context, ?MAX_AGE),
    RD2 = set_cache_control_public(is_public(Info#z_file_info.acls, Context), MaxAge, RD1),
    RD3 = set_content_policy(Info, RD2),
    {z_file_request:content_stream(Info, wrq:resp_content_encoding(RD3)), RD3, State}.


%%%%% -------------------------- Support functions ------------------------

is_noindex(#z_file_info{acls=Acls}, Context) ->
    lists:any(
        fun
            (Id) when is_integer(Id) ->
                CatId = m_rsc:p_no_acl(Id, category_id, Context),
                z_convert:to_bool(m_rsc:p_no_acl(Id, seo_noindex, Context))
                orelse z_convert:to_bool(m_rsc:p_no_acl(CatId, is_seo_noindex_cat, Context));
            (_) ->
                false
        end,
        Acls).

set_content_dispostion(inline, ReqData) ->
    wrq:set_resp_header("Content-Disposition", "inline", ReqData);
set_content_dispostion(attachment, ReqData) ->
    wrq:set_resp_header("Content-Disposition", "attachment", ReqData);
set_content_dispostion(undefined, ReqData) ->
    ReqData.

is_public(Ids, Context) ->
    ContextAnon = z_context:new(Context),
    is_public(Ids, ContextAnon, true).

is_public(_List, _Context, false) ->
    false;
is_public([], _Context, true) ->
    true;
is_public([{module, Mod}|T], Context, _Answer) ->
    is_public(T, Context, z_acl:is_allowed(use, Mod, Context));
is_public([Id|T], Context, _Answer) ->
    is_public(T, Context, z_acl:rsc_visible(Id, Context)).

%% @doc Files that are uploaded get a strict content-security-policy.
%%      Controlled files from the file system are not restricted.
set_content_policy(#z_file_info{ mime = Mime } = Info, ReqData) ->
    IsPlayerNeeded = is_player_needed(Mime),
    case is_resource(Info) of
        true when IsPlayerNeeded ->
            RD1 = wrq:set_resp_header("Content-Security-Policy", "default-src 'none'; media-src 'self'; object-src 'self'", ReqData),
            wrq:set_resp_header("X-Content-Security-Policy", "default-src 'none'; media-src 'self'; object-src 'self'; plugin-types: application/pdf", RD1);
        true ->
            % Do not set the IE11 X-CSP with sandbox as that disables file downloading
            % https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/sandbox
            RD1 = wrq:set_resp_header("Content-Security-Policy", "default-src 'none'; sandbox", ReqData),
            wrq:set_resp_header("X-Content-Security-Policy", "default-src 'none'", RD1);
        false ->
            ReqData
    end.

is_player_needed(<<"application/pdf">>) -> true;
is_player_needed(<<"video/", _/binary>>) -> true;
is_player_needed(<<"audio/", _/binary>>) -> true;
is_player_needed(_) -> false.

%% @doc Check if the served file originated from an user-upload (ie. it is a resource)
is_resource( #z_file_info{ acls = Acls }) ->
    lists:any(fun is_integer/1, Acls).

%% @doc Allow caching on public data, no caching on data that needs access control.
set_cache_control_public(true, MaxAge, ReqData) ->
    wrq:set_resp_header("Cache-Control", "public, max-age="++z_convert:to_list(MaxAge), ReqData);
set_cache_control_public(false, _MaxAge, ReqData) ->
    wrq:set_resp_header("Cache-Control", "private, max-age=0, must-revalidate, post-check=0, pre-check=0", ReqData).

get_file_info(ConfigProps, Context) ->
    get_file_info_cfg(proplists:get_value(path, ConfigProps), ConfigProps, Context).

get_file_info_cfg(undefined, ConfigProps, Context) ->
    DispPath = wrq:disp_path(z_context:get_reqdata(Context)),
    SafePath = mochiweb_util:safe_relative_path(mochiweb_util:unquote(DispPath)),
    get_file_info_path(binpath(SafePath), ConfigProps, Context);
get_file_info_cfg(id, _ConfigProps, _Context) ->
    lager:error("controller_file does not support the 'id' config, use controller_file_id instead."),
    {error, enoent};
get_file_info_cfg(ConfiguredPath, ConfigProps, Context) when is_list(ConfiguredPath); is_binary(ConfiguredPath) ->
    get_file_info_path(binpath(ConfiguredPath), ConfigProps, Context).

get_file_info_path(undefined, _ConfigProps, _Context) ->
    {error, enoent};
get_file_info_path(Path, ConfigProps, Context) when is_binary(Path) ->
    Root = proplists:get_value(root, ConfigProps),
    OptFilters = proplists:get_value(media_tag_url2props, ConfigProps, undefined),
    z_file_request:lookup_file(Path, Root, OptFilters, z_context:set(path, Path, Context)).

%% @doc Ensure path is binary and non-root
binpath(undefined) ->
    <<>>;
binpath(<<"/", Rest/binary>>) ->
    binpath(Rest);
binpath(B) when is_binary(B) ->
    B;
binpath(P) ->
    binpath(z_convert:to_binary(P)).

%% @doc Check if a file is text based (influences the provided character sets)
is_text(<<"text/", _/binary>>) -> true;
is_text(<<"application/x-javascript">>) -> true;
is_text(<<"application/javascript">>) -> true;
is_text(<<"application/xhtml+xml">>) -> true;
is_text(<<"application/xml">>) -> true;
is_text(_Mime) -> false.
