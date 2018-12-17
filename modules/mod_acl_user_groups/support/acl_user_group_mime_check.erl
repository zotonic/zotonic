%% @copyright 2018 Marc Worrell
%% @doc Check mime types against allowed mime types per group

%% Copyright 2018 Marc Worrell
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

-module(acl_user_group_mime_check).

-export([
    mime_allowed_default/1,
    is_acceptable/2
]).


-spec mime_allowed_default( z:context() ) -> binary().
mime_allowed_default(Context) ->
    case m_config:get_value(site, acl_mime_allowed, Context) of
        None when None =:= undefined; None =:= <<>>; None =:= "" ->
            <<"image/*, video/*, audio/*, embed, .pdf, .txt, msoffice, openoffice">>;
        Allowed ->
            z_convert:to_binary(Allowed)
    end.


-spec is_acceptable( binary()|string(), z:context() ) -> boolean().
is_acceptable(Mime, Context) when is_list(Mime) ->
    is_acceptable( list_to_binary(Mime), Context );
is_acceptable(Mime, Context) when is_binary(Mime) ->
    [Type, Sub] = binary:split(Mime, <<"/">>),
    Ext = z_convert:to_binary( z_media_identify:extension(Mime, undefined, Context) ),
    Ids = acl_user_groups_checks:user_groups_all(Context),
    Default = split(mime_allowed_default(Context)),
    lists:any(
        fun(Id) ->
            Allowed = case m_rsc:p_no_acl(Id, acl_mime_allowed, Context) of
                <<>> -> Default;
                undefined -> Default;
                AclAllowed -> split(AclAllowed)
            end,
            lists:any(
                fun(Allow) ->
                    match(Type, Sub, Ext, Allow)
                end,
                Allowed)
        end,
        Ids).

match(_Type, _Sub, _Ext, <<"none">>) -> false;
match(_Type, _Sub, Ext, <<".", _/binary>> = Ext) -> true;
match(_Type, _Sub, _Ext, {<<"*">>, <<"*">>}) -> true;
match(Type,  _Sub, _Ext, {Type, <<"*">>}) -> true;
match(Type,  Sub,  _Ext, {Type, Sub}) -> true;
match(Type,  Sub,  _Ext, <<"msoffice">>) -> match_msoffice(Type, Sub);
match(Type,  Sub,  _Ext, <<"openoffice">>) -> match_openoffice(Type, Sub);
match(Type,  Sub,  _Ext, <<"embed">>) -> match_embed(Type, Sub);
match(_Type, _Sub, _Ext, _) ->
    false.

match_msoffice(<<"application">>, <<"msword">>) -> true;
match_msoffice(<<"application">>, <<"vnd.ms-excel">>) -> true;
match_msoffice(<<"application">>, <<"vnd.ms-powerpoint">>) -> true;
match_msoffice(<<"application">>, <<"vnd.ms-project">>) -> true;
match_msoffice(<<"application">>, <<"vnd.openxmlformats-officedocument.", _/binary>>) -> true;
match_msoffice(<<"application">>, <<"vnd.visio">>) -> true;
match_msoffice(<<"application">>, <<"x-msaccess">>) -> true;
match_msoffice(_, _) -> false.

match_openoffice(<<"application">>, <<"vnd.oasis.opendocument.", _/binary>>) -> true;
match_openoffice(_, _) -> false.

match_embed(<<"text">>, <<"html-oembed">>) -> true;         % mod_oembed
match_embed(<<"text">>, <<"html-video-embed">>) -> true;    % mod_video_embed
match_embed(_, _) -> false.

split(Allowed) ->
    All = binary:split(Allowed, <<",">>, [global]),
    lists:flatten(
        lists:map(
            fun(Mime) ->
                case binary:split(z_string:trim(Mime), <<"/">>) of
                    [Type, Sub] -> {Type, Sub};
                    Type -> Type
                end
            end,
            All)).

