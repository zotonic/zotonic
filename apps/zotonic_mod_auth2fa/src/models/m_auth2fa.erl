%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Generate TOTP image data: urls.

%% Copyright 2019 Marc Worrell
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

    request_key/1,
    set_request_key/1,

    is_totp_enabled/2,
    is_valid_totp/3,

    user_mode/1,

    totp_image_url/2,
    totp_disable/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../support/z_auth2fa_qrcode.hrl").

-define(TOTP_PERIOD, 30).
-define(TOTP_IDENTITY_TYPE, auth2fa_totp).
-define(TOTP_IDENTITY_REQUEST_KEY, auth2fa_request_key).

m_get([ totp_image_url, RequestKey | Rest ], _Msg, Context) ->
    case request_key(Context) of
        undefined ->
            {ok, {<<>>, Rest}};
        RequestKey ->
            reset_request_key(Context),
            {ok, {totp_image_url(z_acl:user(Context), Context), Rest}}
    end;
m_get([ User, is_totp_enabled | Rest ], _Msg, Context) ->
    UserId = m_rsc:rid(User, Context),
    IsEnabled = case z_acl:is_allowed(use, mod_admin_identity, Context)
        orelse UserId =:= z_acl:user(Context)
    of
        true -> is_totp_enabled(UserId, Context);
        false -> undefined
    end,
    {ok, {IsEnabled, Rest}};
m_get([ is_totp_enabled | Rest ], _Msg, Context) ->
    IsEnabled  = case z_acl:user(Context) of
        undefined -> false;
        UserId -> is_totp_enabled(UserId, Context)
    end,
    {ok, {IsEnabled, Rest}};
m_get([ is_totp_requested, RequestKey | Rest ], _Msg, Context) ->
    IsRequested = case request_key(Context) of
        undefined -> false;
        RequestKey -> true;
        _ -> false
    end,
    {ok, {IsRequested, Rest}};
m_get([ user_mode | Rest ], _Msg, Context) ->
    {ok, {user_mode(Context), Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

-spec request_key( z:context() ) -> binary() | undefined.
request_key( Context ) ->
    case z_acl:user(Context) of
        undefined ->
            undefined;
        UserId ->
            case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_REQUEST_KEY, Context) of
                [] -> undefined;
                [Idn] -> proplists:get_value(key, Idn)
            end
    end.

-spec set_request_key( z:context() ) -> binary() | undefined.
set_request_key( Context ) ->
    case z_acl:user(Context) of
        undefined ->
            undefined;
        UserId ->
            Key = z_ids:id(),
            {ok, _} = m_identity:insert_single(UserId, ?TOTP_IDENTITY_REQUEST_KEY, Key, Context),
            Key
    end.

-spec reset_request_key( z:context() ) -> ok.
reset_request_key( Context ) ->
    case z_acl:user(Context) of
        undefined ->
            ok;
        UserId ->
            _ = m_identity:delete_by_type(UserId, ?TOTP_IDENTITY_REQUEST_KEY, Context),
            ok
    end.



%% @doc Check if totp is enabled for the given user
-spec is_totp_enabled( m_rsc:resource_id(), z:context() ) -> boolean().
is_totp_enabled(UserId, Context) ->
    case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context) of
        [] -> false;
        [_] -> true
    end.

%% @doc Check the totp mode for the current user: 0 = optional, 1 = ask, 2 = required
-spec user_mode( z:context() ) -> 0 | 1 | 2.
user_mode(Context) ->
    case z_auth:is_auth(Context) of
        true ->
            case z_convert:to_integer(m_config:get_value(mod_auth2fa, mode, Context)) of
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
-spec totp_disable( m_rsc:resource_id(), z:context() ) -> ok.
totp_disable(UserId, Context) ->
    m_identity:delete_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context).

%% @doc Generate a new totp code and return the barcode
-spec totp_image_url( m_rsc:resource_id(), z:context() ) -> binary().
totp_image_url(UserId, Context) when is_integer(UserId) ->
    case is_allowed_totp_enable(UserId, Context) of
        true ->
            Issuer = z_convert:to_binary( z_context:hostname(Context) ),
            Username = m_identity:get_username(Context),
            ServicePart = iolist_to_binary([
                    Issuer,
                    <<"%3A">>,
                    z_url:url_encode(Username), <<"%20%2F%20">>, Issuer
                ]),
            {ok, Passcode} = regenerate_user_secret(UserId, Context),
            {ok, Png} = generate_png(ServicePart, Issuer, Passcode, ?TOTP_PERIOD),
            encode_data_url(Png, <<"image/png">>);
        false ->
            <<>>
    end.

%% Only the admin user can enable totp for the admin user
is_allowed_totp_enable(1, Context) ->
    z_acl:user(Context) =:= 1;
is_allowed_totp_enable(UserId, Context) ->
    z_acl:user(Context) =:= UserId
    orelse z_acl:is_allowed(use, mod_admin_identity, Context).


encode_data_url(Data, Mime) ->
    iolist_to_binary([ <<"data:">>, Mime, <<";base64,">>, base64:encode(Data) ]).


%% @doc Check if the given code is a valid TOTP code
-spec is_valid_totp( m_rsc:resource_id(), binary(), z:context() ) -> boolean().
is_valid_totp(UserId, Code, Context) when is_integer(UserId), is_binary(Code) ->
    case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context) of
        [Idn] ->
            Passcode = proplists:get_value(propb, Idn),
            {A, B, C} = totp(Passcode, ?TOTP_PERIOD),
            case Code of
                A -> true;
                B -> true;
                C -> true;
                _ -> false
            end;
        [] ->
            false
    end.

regenerate_user_secret(UserId, Context) ->
    F = fun(Ctx) ->
        totp_disable(UserId, Context),
        Passcode = crypto:hash(sha, z_ids:id(32)),
        Props = [
            {propb, {term, Passcode}}
        ],
        m_identity:insert(UserId, ?TOTP_IDENTITY_TYPE, <<>>, Props, Ctx),
        {ok, Passcode}
    end,
    z_db:transaction(F, Context).

% url format: https://github.com/google/google-authenticator/wiki/Key-Uri-Format
generate_png(Domain, Issuer, Passcode, Seconds) ->
    PasscodeBase32 = z_auth2fa_base32:encode(Passcode),
    Period = integer_to_binary(Seconds),
    Token = iolist_to_binary([
        "otpauth://totp/", Domain,
        "?period=", Period,
        "&issuer=", z_url:url_encode(Issuer),
        "&secret=", PasscodeBase32
    ]),
    QRCode = z_auth2fa_qrcode:encode(Token),
    Image = simple_png_encode(QRCode),
    {ok, Image}.

%% Very simple PNG encoder for demo purposes
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
    HS = crypto:hmac(sha, Key, <<Count:64>>),
    <<_:19/binary, _:4, Offset:4>> = HS,
    <<_:Offset/binary, _:1, P:31, _/binary>> = HS,
    HOTP = integer_to_list(P rem 1000000),
    Pad = lists:duplicate(6 - length(HOTP), $0),
    list_to_binary([Pad, HOTP]).
