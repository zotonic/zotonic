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
    service_available/1,
    allowed_methods/1,
    resource_exists/1,
    forbidden/1,
    last_modified/1,
    expires/1,
    content_types_provided/1,
    charsets_provided/1,
    content_encodings_provided/1,
    provide_content/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

-define(MAX_AGE, 31536000).


%% @doc Initialize the context for the request. Optionally continue the user's session.
service_available(Context0) ->
    Context = z_context:set_noindex_header(Context0),
    Context1 = z_context:ensure_qs(Context),
    z_context:lager_md(Context1),
    case get_file_info(Context1) of
        {ok, Info} ->
            {true, z_context:set(?MODULE, Info, Context1)};
        {error, enoent} = Error ->
            {true, z_context:set(?MODULE, Error, Context1)};
        {error, _} = Error ->
            {false, z_context:set(?MODULE, Error, Context1)}
    end.

allowed_methods(Context) ->
    {[<<"HEAD">>, <<"GET">>], Context}.

resource_exists(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {false, Context};
        #z_file_info{acls=Acls} ->
            {not lists:any(fun(Id) when is_integer(Id) ->
                                    not m_rsc:exists(Id, Context);
                              ({module, _Module}) ->
                                    false
                           end,
                           Acls), Context}
    end.

forbidden(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _ } ->
            {false, Context};
        #z_file_info{} = FInfo ->
            case z_controller_helper:is_authorized(Context) of
                {false, Context2} ->
                    {true, Context2};
                {true, Context2} ->
                    {not z_file_request:is_visible(FInfo, Context2), Context2}
            end
    end.

last_modified(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {calendar:universal_time(), Context};
        #z_file_info{modifiedUTC=LModUTC} ->
            {LModUTC, Context}
    end.

expires(Context) ->
    Date = calendar:universal_time(),
    case z_context:get(max_age, Context, ?MAX_AGE) of
        N when N >= ?MAX_AGE ->
            {z_datetime:next_year(Date), Context};
        MaxAge ->
            NowSecs = calendar:datetime_to_gregorian_seconds(Date),
            {calendar:gregorian_seconds_to_datetime(NowSecs + MaxAge), Context}
    end.

charsets_provided(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {no_charset, Context};
        #z_file_info{mime=Mime} ->
            case is_text(Mime) of
                true -> {[<<"utf-8">>], Context};
                _ -> {no_charset, Context}
            end
    end.

content_encodings_provided(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {[<<"identity">>], Context};
        #z_file_info{} = Info ->
            Encs = z_file_request:content_encodings(Info),
            {[z_convert:to_binary(Enc)||Enc<-Encs], Context}
    end.

content_types_provided(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {[{<<"text/plain">>, provide_content}], Context};
        #z_file_info{mime=Mime} ->
            {[{z_convert:to_binary(Mime), provide_content}], Context}
    end.

provide_content(Context) ->
    case z_context:get(?MODULE, Context) of
        {error, _} ->
            {<<>>, Context};
        #z_file_info{} = Info ->
            Context1 = set_content_dispostion(z_context:get(content_disposition, Context), Context),
            MaxAge = z_context:get(max_age, Context1, ?MAX_AGE),
            Context2 = set_cache_control_public(is_public(Info#z_file_info.acls, Context1), MaxAge, Context1),
            Context3 = set_allow_origin(Context2),
            Context4 = set_content_policy(Info, Context3),
            {z_file_request:content_stream(Info, cowmachine_req:resp_content_encoding(Context4)), Context4}
    end.

%%%%% -------------------------- Support functions ------------------------

set_content_dispostion(inline, Context) ->
    z_context:set_resp_header(<<"content-disposition">>, <<"inline">>, Context);
set_content_dispostion(attachment, Context) ->
    z_context:set_resp_header(<<"content-disposition">>, <<"attachment">>, Context);
set_content_dispostion(undefined, Context) ->
    Context.

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

set_allow_origin(Context) ->
    z_context:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Context).

set_content_policy(#z_file_info{acls=[]}, Context) ->
    Context;
set_content_policy(#z_file_info{acls = Acls, mime = <<"application/pdf">>}, Context) ->
    case lists:any(fun is_integer/1, Acls) of
        true ->
            Context1 = z_context:set_resp_header(
                    <<"content-security-policy">>,
                    <<"object-src 'self'; plugin-types application/pdf">>,
                    Context),
            z_context:set_resp_header(
                    <<"x-content-security-policy">>,
                    <<"plugin-types: application/pdf">>,
                    Context1);
        false ->
            Context
    end;
set_content_policy(#z_file_info{acls=Acls}, Context) ->
    case lists:any(fun is_integer/1, Acls) of
        true ->
            Context1 = z_context:set_resp_header(<<"content-security-policy">>, <<"sandbox">>, Context),
            % IE11 needs the X- variant, see http://caniuse.com/#feat=contentsecuritypolicy
            z_context:set_resp_header(<<"x-content-security-policy">>, <<"sandbox">>, Context1);
        false ->
            Context
    end.

set_cache_control_public(true, MaxAge, Context) ->
    z_context:set_resp_header(
            <<"cache-control">>,
            <<"public, max-age=", (z_convert:to_binary(MaxAge))/binary>>,
            Context);
set_cache_control_public(false, _MaxAge, Context) ->
    z_context:set_resp_header(
            <<"cache-control">>,
            <<"private, max-age=0, must-revalidate, post-check=0, pre-check=0">>,
            Context).

get_file_info(Context) ->
    get_file_info_cfg(z_context:get(path, Context), Context).

get_file_info_cfg(undefined, Context) ->
    DispPath = drop_dotdot(maybe_urldecode(rootless(cowmachine_req:disp_path(Context)))),
    get_file_info_path(DispPath, Context);
get_file_info_cfg(id, _Context) ->
    lager:error("controller_file does not support the 'id' config, use controller_file_id instead."),
    {error, enoent};
get_file_info_cfg(ConfiguredPath, Context) when is_list(ConfiguredPath); is_binary(ConfiguredPath) ->
    get_file_info_path(rootless(z_convert:to_binary(ConfiguredPath)), Context).

get_file_info_path(undefined, _Context) ->
    {error, enoent};
get_file_info_path(Path, Context) when is_binary(Path) ->
    Root = z_context:get(root, Context),
    OptFilters = z_context:get(media_tag_url2props, Context, undefined),
    z_file_request:lookup_file(Path, Root, OptFilters, z_context:set(path, Path, Context)).

%% @doc Ensure path is non-root
rootless(undefined) -> <<>>;
rootless(<<"/", Rest/binary>>) -> rootless(Rest);
rootless(<<"./", Rest/binary>>) -> rootless(Rest);
rootless(<<"../", Rest/binary>>) -> rootless(Rest);
rootless(B) when is_binary(B) -> B.

maybe_urldecode(Path) ->
    case binary:match(Path, <<"%">>) of
        nomatch -> Path;
        {_,_} -> rootless(cow_qs:urldecode(Path))
    end.

drop_dotdot(Path) ->
    case binary:match(Path, <<"../">>) =/= nomatch
        orelse binary:match(Path, <<"./">>) =/= nomatch
    of
        false ->
            Path;
        true ->
            Parts = binary:split(Path, <<"/">>, [global]),
            Parts1 = drop_dotdot_1(Parts, []),
            rootless(iolist_to_binary(z_utils:combine($/, Parts1)))
    end.

drop_dotdot_1([], Acc) ->
    lists:reverse(Acc);
drop_dotdot_1([_Dir,<<"..">>|Rest], Acc) ->
    drop_dotdot_1(Rest, Acc);
drop_dotdot_1([<<"..">>|Rest], Acc) ->
    drop_dotdot_1(Rest, Acc);
drop_dotdot_1([<<".">>|Rest], Acc) ->
    drop_dotdot_1(Rest, Acc);
drop_dotdot_1([Other|Rest], Acc) ->
    drop_dotdot_1(Rest, [Other|Acc]).


%% @doc Check if a file is text based (influences the provided character sets)
is_text(<<"text/", _/binary>>) -> true;
is_text(<<"application/x-javascript">>) -> true;
is_text(<<"application/javascript">>) -> true;
is_text(<<"application/xhtml+xml">>) -> true;
is_text(<<"application/xml">>) -> true;
is_text(_Mime) -> false.
