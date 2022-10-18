%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2022 Marc Worrell
%% @doc Generate TOTP image data: urls.

%% Copyright 2019-2022 Marc Worrell
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

-export([
    m_find_value/3,

    is_totp_enabled/2,
    is_valid_totp/3,

    is_valid_totp_test/2,

    user_mode/1,
    user_mode/2,

    totp_image_url/2,
    totp_disable/2,
    totp_set/3
]).

-include("zotonic.hrl").
-include("../support/z_auth2fa_qrcode.hrl").

-define(TOTP_PERIOD, 30).
-define(TOTP_IDENTITY_TYPE, <<"auth2fa_totp">>).

m_find_value(totp_image_url, #m{ value = undefined }, Context) ->
    case z_acl:user(Context) of
        undefined ->
            undefined;
        UserId ->
            case z_context:get_session(request_2fa_user_id, Context) of
                UserId ->
                    case totp_image_url(z_acl:user(Context), Context) of
                        {ok, {Url, Secret}} ->
                            #{
                                url => Url,
                                secret => z_auth2fa_base32:encode(Secret)
                            };
                        {error, _} ->
                            undefined
                    end;
                _ ->
                    undefined
            end
    end;
m_find_value(new_totp_image_url, #m{ value = undefined }, Context) ->
    case new_totp_image_url(Context) of
        {ok, {Url, Secret}} ->
            #{
                url => Url,
                secret => z_auth2fa_base32:encode(Secret)
            };
        {error, _} ->
            undefined
    end;
m_find_value(is_totp_enabled, #m{ value = undefined }, Context) ->
    case z_acl:user(Context) of
        undefined -> false;
        UserId -> is_totp_enabled(UserId, Context)
    end;
m_find_value(is_totp_enabled, #m{ value = UserId }, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context)
        orelse UserId =:= z_acl:user(Context)
    of
        true -> is_totp_enabled(UserId, Context);
        false -> undefined
    end;

m_find_value(is_totp_requested, #m{ value = undefined }, Context) ->
    case z_acl:user(Context) of
        undefined ->
            z_context:set_session(request_2fa_user_id, undefined, Context),
            false;
        UserId ->
            case z_context:get_session(request_2fa_user_id, Context) of
                UserId -> true;
                _ -> false
            end
    end;

m_find_value(user_mode, #m{ value = undefined }, Context) ->
    user_mode(Context);
m_find_value(user_mode, #m{ value = UserId }, Context) when is_integer(UserId) ->
    user_mode(UserId, Context);

m_find_value(UserId, #m{ value = undefined } = M, _Context) when is_integer(UserId) ->
    M#m{ value = UserId }.


%% @doc Check if totp is enabled for the given user
-spec is_totp_enabled( m_rsc:resource_id(), z:context() ) -> boolean().
is_totp_enabled(UserId, Context) ->
    case m_identity:get_rsc_by_type(UserId, ?TOTP_IDENTITY_TYPE, Context) of
        [] -> false;
        [_] -> true
    end.

%% @doc Check the totp mode for the current user: 0 = optional, 1 = ask, 2 = required, 3 = forced
-spec user_mode( z:context() ) -> 0 | 1 | 2 | 3.
user_mode(Context) ->
    user_mode(z_acl:user(Context), Context).

user_mode(undefined, _Context) ->
    0;
user_mode(UserId, Context) ->
    case z_convert:to_integer(m_config:get_value(mod_auth2fa, mode, Context)) of
        3 -> 3;
        2 -> erlang:max( user_group_mode(UserId, Context), 2 );
        1 -> erlang:max( user_group_mode(UserId, Context), 1 );
        _ -> erlang:max( user_group_mode(UserId, Context), 0 )
    end.

user_group_mode(UserId, Context) ->
    case z_module_manager:active(mod_acl_user_groups, Context) of
        true ->
            UGIds = m_acl_user_group:user_groups(UserId, Context),
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

%% @doc Set the totp token for the user
-spec totp_set( m_rsc:resource_id(), Passcode::string()|binary(), z:context() ) -> ok | {error, already_set}.
totp_set(UserId, Passcode, Context) ->
    case is_totp_enabled(UserId, Context) of
        true ->
            {error, already_set};
        false ->
            {ok, _} = set_user_secret(UserId, Passcode, Context),
            ok
    end.

%% @doc Generate a new totp code and return the barcode, save it for the user.
-spec totp_image_url( m_rsc:resource_id(), z:context() ) -> {ok, {binary(), binary()}} | {error, eacces}.
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
            {ok, {encode_data_url(Png, <<"image/png">>), Passcode}};
        false ->
            {error, eacces}
    end.

%% @doc Generate a new totp code and return the barcode, do not save it.
-spec new_totp_image_url( z:context() ) -> {ok, {binary(), binary()}} | {error, eacces}.
new_totp_image_url(Context) ->
    Issuer = z_convert:to_binary( z_context:hostname(Context) ),
    Title = z_convert:to_binary(m_site:get(title, Context)),
    ServicePart = iolist_to_binary([
        Issuer,
        <<"%3A">>,
        z_url:url_encode(Title),
        case Issuer of
            Title -> <<>>;
            _ -> [ <<"%20%2F%20">>, Issuer ]
        end
    ]),
    Passcode = new_secret(),
    {ok, Png} = generate_png(ServicePart, Issuer, Passcode, ?TOTP_PERIOD),
    {ok, {encode_data_url(Png, <<"image/png">>), Passcode}}.

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
            is_valid_totp_test(Passcode, Code);
        [] ->
            false
    end.

%% @doc Check if the given code is a valid TOTP code
-spec is_valid_totp_test( Secret::string()|binary(), Code::string()|binary() ) -> boolean().
is_valid_totp_test(Secret, Code) ->
    {A, B, C} = totp(z_convert:to_binary(Secret), ?TOTP_PERIOD),
    case z_convert:to_binary(Code) of
        A -> true;
        B -> true;
        C -> true;
        _ -> false
    end.

regenerate_user_secret(UserId, Context) ->
    Passcode = new_secret(),
    set_user_secret(UserId, Passcode, Context).

set_user_secret(UserId, Passcode, Context) ->
    F = fun(Ctx) ->
        totp_disable(UserId, Ctx),
        Props = [
            {propb, {term, Passcode}}
        ],
        m_identity:insert(UserId, ?TOTP_IDENTITY_TYPE, <<>>, Props, Ctx),
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
