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
    Context = z_context:set_noindex_header(z_context:set(ConfigProps, z_context:new(ReqData))),
    Context1 = z_context:continue_session(z_context:ensure_qs(Context)),
    case get_file_info(ConfigProps, Context1) of
        {ok, Info} ->
            {true, ReqData, {Info, Context1}};
        {error, enoent} = Error ->
            {true, ReqData, {Error, Context1}};
        {error, _} = Error ->
            {false, ReqData, {Error, Context1}}
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
forbidden(ReqData, {#z_file_info{acls=Acls},Context} = State) ->
    {lists:any(fun(Id) when is_integer(Id) ->
                        not z_acl:rsc_visible(Id, Context);
                  ({module, Module}) ->
                        Module:file_forbidden(z_context:get(path, Context), Context)
               end, 
               Acls), ReqData, State}.

last_modified(ReqData, {{error, _},_} = State) ->
    {calendar:universal_time(), ReqData, State};
last_modified(ReqData, {#z_file_info{modifiedUTC=LModUTC},_Context} = State) ->
    {LModUTC, ReqData, State}.

expires(ReqData, State) ->
    {{Y,M,D},HIS} = calendar:universal_time(),
    {{{Y+1,M,D},HIS}, ReqData, State}.

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
    RD2 = set_cache_control_public(is_public(Info#z_file_info.acls, Context), RD1),
    {z_file_request:content_stream(Info, wrq:resp_content_encoding(RD2)), RD2, State}.


%%%%% -------------------------- Support functions ------------------------

set_content_dispostion(inline, ReqData) ->
    wrq:set_resp_header("Content-Disposition", "inline", ReqData);
set_content_dispostion(attachment, ReqData) ->
    wrq:set_resp_header("Content-Disposition", "attachment", ReqData);
set_content_dispostion(undefined, ReqData) ->
    ReqData.

is_public([], _Context) ->
    true;
is_public(Ids, Context) ->
    ContextAnon = z_context:new(Context),
    not lists:any(fun(Id) -> not z_acl:rsc_visible(Id, ContextAnon) end, Ids).

set_cache_control_public(true, ReqData) ->
    wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData);
set_cache_control_public(false, ReqData) ->
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
is_text(<<"application/xhtml+xml">>) -> true;
is_text(<<"application/xml">>) -> true;
is_text(_Mime) -> false.
