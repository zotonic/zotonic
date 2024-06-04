%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Marc Worrell
%% @doc Generate TOTP image data: urls and manage TOTP user secrets.
%% @end

%% Copyright 2019-2024 Marc Worrell
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

-module(m_auth2fa).

-behaviour(zotonic_model).

-export([
    m_get/3,

    set_totp_requested/1,
    is_totp_requested/1,

    is_totp_enabled/2,
    is_valid_totp/3,

    is_valid_totp_test/2,

    mode/1,
    user_mode/1,

    new_totp_image_url/1,
    totp_disable/2,
    totp_set/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../support/z_auth2fa_qrcode.hrl").

-define(TOTP_PERIOD, 30).
-define(TOTP_IDENTITY_TYPE, auth2fa_totp).

m_get([ <<"new_totp_image_url">> | Rest ], _Msg, Context) ->
    {ok, {ImageDataUrl, Secret}} = new_totp_image_url(Context),
    R = #{
        url => ImageDataUrl,
        secret => z_auth2fa_base32:encode(Secret)
    },
    {ok, {R, Rest}};
m_get([ User, <<"is_totp_enabled">> | Rest ], _Msg, Context) ->
    UserId = m_rsc:rid(User, Context),
    IsEnabled = case z_acl:is_allowed(use, mod_admin_identity, Context)
        orelse UserId =:= z_acl:user(Context)
    of
        true -> is_totp_enabled(UserId, Context);
        false -> undefined
    end,
    {ok, {IsEnabled, Rest}};
m_get([ <<"is_totp_enabled">> | Rest ], _Msg, Context) ->
    IsEnabled  = case z_acl:user(Context) of
        undefined -> false;
        UserId -> is_totp_enabled(UserId, Context)
    end,
    {ok, {IsEnabled, Rest}};
m_get([ <<"is_totp_requested">> | Rest ], _Msg, Context) ->
    {ok, {is_totp_requested(Context), Rest}};
m_get([ <<"mode">> | Rest ], _Msg, Context) ->
    {ok, {mode(Context), Rest}};
m_get([ <<"user_mode">> | Rest ], _Msg, Context) ->
    {ok, {user_mode(Context), Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Remember that for this session the TOTP dialog has been shown.
-spec is_totp_requested(Context) -> boolean() when
    Context :: z:context().
is_totp_requested(Context) ->
    case z_notifier:first({server_storage, lookup, is_totp_requested}, Context) of
        {ok, true} -> true;
        _ -> false
    end.

%% @doc Check if for this session the TOTP dialog has been shown.
-spec set_totp_requested(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: server_storage | no_session | not_found | full | term().
set_totp_requested(Context) ->
    case z_notifier:first({server_storage, store, is_totp_requested, true}, Context) of
        undefined -> {error, server_storage};
        ok -> ok;
        {error, _} = Error -> Error
    end.

%% @doc Check if totp is enabled for the given user
-spec is_totp_enabled( m_rsc:resource_id(), z:context() ) -> boolean().
is_totp_enabled(UserId, Context) ->
    case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context) of
        [] -> false;
        [_] -> true
    end.

%% @doc Check the totp mode
-spec mode( z:context() ) -> 0 | 1 | 2 | 3.
mode(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_auth2fa, mode, Context)) of
        3 -> 3;
        2 -> 2;
        1 -> 1;
        _ -> 0
    end.

%% @doc Check the totp mode for the current user:
%% 0 = optional, 1 = ask, 2 = required, 3 = forced
-spec user_mode( z:context() ) -> 0 | 1 | 2 | 3.
user_mode(Context) ->
    case z_auth:is_auth(Context) of
        true ->
            case z_convert:to_integer(m_config:get_value(mod_auth2fa, mode, Context)) of
                3 -> 3;
                2 -> 2;
                1 -> erlang:max( user_group_mode(Context), 1 );
                _ -> erlang:max( user_group_mode(Context), 0 )
            end;
        false ->
            0
    end.

user_group_mode(Context) ->
    case z_module_manager:active(mod_acl_user_groups, Context) of
        true ->
            UGIds = m_acl_user_group:user_groups(Context),
            Modes = lists:map(
                fun(Id) ->
                    case m_rsc:p_no_acl(Id, acl_2fa, Context) of
                        undefined -> 0;
                        <<>> -> 0;
                        N -> z_convert:to_integer(N)
                    end
                end,
                UGIds),
            lists:max(Modes);
        false ->
            0
    end.

%% @doc Remove the totp tokens and disable totp for the user
-spec totp_disable(UserId, Context) -> ok when
    UserId :: m_rsc:resource_id(),
    Context :: z:context().
totp_disable(UserId, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_auth2fa,
        text => <<"2FA code removed for user">>,
        for_user_id => UserId,
        by_user_id => z_acl:user(Context)
    }),
    m_identity:delete_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context).

%% @doc Set the totp token for the user
-spec totp_set(UserId, Secret, Context) -> ok | {error, already_set} when
    UserId :: m_rsc:resource_id(),
    Secret :: binary(),
    Context :: z:context().
totp_set(UserId, Secret, Context) ->
    case is_totp_enabled(UserId, Context) of
        true ->
            {error, already_set};
        false ->
            {ok, _} = set_user_secret(UserId, Secret, Context),
            ok
    end.

%% @doc Generate a new totp code and return the barcode, do not save it.
-spec new_totp_image_url(Context) -> {ok, {Url, Secret}} when
    Context :: z:context(),
    Url :: binary(),
    Secret :: binary().
new_totp_image_url(Context) ->
    Issuer = issuer(Context),
    ServicePart = service_part(z_acl:user(Context), Issuer, Context),
    Passcode = new_secret(),
    {ok, Png} = generate_png(ServicePart, Issuer, Passcode, ?TOTP_PERIOD),
    {ok, {encode_data_url(Png, <<"image/png">>), Passcode}}.

issuer(Context) ->
    SiteTitle = m_config:get_value(site, title, Context),
    Issuer = case z_utils:is_empty(SiteTitle) of
        true -> z_context:hostname(Context);
        false -> SiteTitle
    end,
    z_convert:to_binary(Issuer).

service_part(undefined, Issuer, _Context) ->
    url_encode(
        iolist_to_binary([
            Issuer,
            $:,
            Issuer
        ]));
service_part(UserId, Issuer, Context) ->
    Username = z_convert:to_binary( m_identity:get_username(UserId, Context) ),
    url_encode(
        iolist_to_binary([
            Issuer,
            $:,
            Username, " / ", Issuer
        ])).

encode_data_url(Data, Mime) ->
    iolist_to_binary([ <<"data:">>, Mime, <<";base64,">>, base64:encode(Data) ]).

url_encode(S) ->
    binary:replace(z_url:url_encode(S), <<"+">>, <<"%20">>, [ global ]).

%% @doc Check if the given code is a valid TOTP code for the secret stored with
%% the given user.
-spec is_valid_totp(UserId, Code, Context) -> boolean() when
    UserId :: m_rsc:resource_id(),
    Code :: binary() | string() | integer(),
    Context :: z:context().
is_valid_totp(UserId, Code, Context) when is_integer(UserId), is_binary(Code) ->
    case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context) of
        [Idn] ->
            Passcode = proplists:get_value(propb, Idn),
            is_valid_totp_test(Passcode, Code);
        [] ->
            false
    end.

%% @doc Check if the given code is a valid TOTP code for the given secret.
-spec is_valid_totp_test(Secret, Code) -> boolean() when
    Secret :: binary(),
    Code :: string() | binary() | integer().
is_valid_totp_test(Secret, Code) ->
    {A, B, C} = totp(Secret, ?TOTP_PERIOD),
    case z_convert:to_binary(Code) of
        A -> true;
        B -> true;
        C -> true;
        _ -> false
    end.

set_user_secret(UserId, Passcode, Context) ->
    F = fun(Ctx) ->
        totp_disable(UserId, Context),
        Props = [
            {propb, {term, Passcode}}
        ],
        m_identity:insert(UserId, ?TOTP_IDENTITY_TYPE, <<>>, Props, Ctx),
        ?LOG_INFO(#{
            in => zotonic_mod_auth2fa,
            text => <<"2FA code added for user">>,
            for_user_id => UserId,
            by_user_id => z_acl:user(Context)
        }),
        {ok, Passcode}
    end,
    z_db:transaction(F, Context).

new_secret() ->
    crypto:hash(sha, z_ids:id(32)).

% url format: https://github.com/google/google-authenticator/wiki/Key-Uri-Format
generate_png(Domain, Issuer, Passcode, Seconds) ->
    PasscodeBase32 = z_auth2fa_base32:encode(Passcode),
    Period = integer_to_binary(Seconds),
    Token = iolist_to_binary([
        "otpauth://totp/", Domain,
        "?period=", Period,
        "&issuer=", url_encode(Issuer),
        "&secret=", PasscodeBase32
    ]),
    QRCode = z_auth2fa_qrcode:encode(Token),
    Image = simple_png_encode(QRCode),
    {ok, Image}.

%% Very simple PNG encoder
simple_png_encode(#qrcode{ dimension = Dim, data = Data }) ->
    MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
    Size = Dim * 8,
    IHDR = png_chunk(<<"IHDR">>, <<Size:32, Size:32, 8:8, 2:8, 0:24>>),
    PixelData = get_pixel_data(Dim, Data),
    IDAT = png_chunk(<<"IDAT">>, PixelData),
    IEND = png_chunk(<<"IEND">>, <<>>),
    <<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

png_chunk(Type, Bin) ->
    Length = byte_size(Bin),
    CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
    <<Length:32, Type/binary, Bin/binary, CRC:32>>.

get_pixel_data(Dim, Data) ->
    Pixels = get_pixels(Data, 0, Dim, <<>>),
    zlib:compress(Pixels).

get_pixels(<<>>, Dim, Dim, Acc) ->
    Acc;
get_pixels(Bin, Count, Dim, Acc) ->
    <<RowBits:Dim/bits, Bits/bits>> = Bin,
    Row = get_pixels0(RowBits, <<0>>), % row filter byte
    FullRow = binary:copy(Row, 8),
    get_pixels(Bits, Count + 1, Dim, <<Acc/binary, FullRow/binary>>).

get_pixels0(<<1:1, Bits/bits>>, Acc) ->
    Black = binary:copy(<<0>>, 24),
    get_pixels0(Bits, <<Acc/binary, Black/binary>>);
get_pixels0(<<0:1, Bits/bits>>, Acc) ->
    White = binary:copy(<<255>>, 24),
    get_pixels0(Bits, <<Acc/binary, White/binary>>);
get_pixels0(<<>>, Acc) ->
    Acc.

%% @doc Generate the three acceptable totp codes for the given key.
-spec totp( binary(), pos_integer() ) -> {binary(), binary(), binary()}.
totp(Key, Period) ->
    T = z_datetime:timestamp() div Period,
    {hotp(Key, T - 1), hotp(Key, T), hotp(Key, T + 1)}.

%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% See <http://tools.ietf.org/html/rfc4226>
-spec hotp( binary(), pos_integer() ) -> binary().
hotp(Key, Count) when is_binary(Key), is_integer(Count) ->
    HS = crypto:mac(hmac, sha, Key, <<Count:64>>),
    <<_:19/binary, _:4, Offset:4>> = HS,
    <<_:Offset/binary, _:1, P:31, _/binary>> = HS,
    HOTP = integer_to_list(P rem 1000000),
    Pad = lists:duplicate(6 - length(HOTP), $0),
    list_to_binary([Pad, HOTP]).
