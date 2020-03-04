%% @author Marc Worrell <marc@worrell.nl>
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2020 Marc Worrell, Maas-Maarten Zeeman
%% @doc SSL support functions, ensure the DH file.

%% Copyright 2020 Marc Worrell, Maas-Maarten Zeeman
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

-module(z_ssl_dhfile).
-author('Marc Worrell <marc@worrell.nl>').
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-export([
    dh_options/0,
    dhfile/0,
    is_dhfile/1,
    ensure_dhfile/0
]).

-define(DEFAULT_DHGROUP, ffdhe3072).

%% @doc Return the dh key to be used. Needed for better forward secrecy
%%      with the DH key exchange
-spec dh_options() -> [ssl:ssl_option()].
dh_options() ->
    [ {dhfile, dhfile()} ].

is_dhfile(Filename) ->
    case file:read_file(Filename) of
        {ok, <<"-----BEGIN DH PARAMETERS", _/binary>>} -> true;
        _ -> false
    end.

ensure_dhfile() ->
    ensure_dhfile(dhfile()).

dhfile() ->
    case z_config:get(ssl_dhfile) of
        undefined ->
            {ok, SecurityDir} = z_config_files:security_dir(),
            DeprecatedFilename = filename:join([ SecurityDir, "dh2048.pem" ]),
            case filelib:is_file(DeprecatedFilename) of
                true ->
                    DeprecatedFilename;
                false ->
                    Param = z_convert:to_list(z_config:get(ssl_dhgroup, ?DEFAULT_DHGROUP)),
                    filename:join([ SecurityDir, "dh-"++Param++".pem" ])
            end;
        Filename ->
            Filename
    end.

ensure_dhfile(Filename) ->
    case filelib:is_file(Filename) of
        true ->
            ok;
        false ->
            ok = z_filelib:ensure_dir(Filename),
            write_dhfile(Filename)
    end.

write_dhfile(Filename) ->
    Param = z_convert:to_atom(z_config:get(ssl_dhgroup, ?DEFAULT_DHGROUP)),
    lager:info("Writing DH key ~p to '~s'",
               [Param, Filename]),
    case file:write_file(Filename, dh_params(Param)) of
        ok ->
            _ = file:change_mode(Filename, 8#00600),
            ok;
        {error, Reason} ->
            lager:error("Failed writing DH file in '~s' (error was ~p)",
                        [Filename, Reason]),
            {error, dhfile}
    end.

%%
%% Recommended pre-configured DH groups per IETF (RFC 7919:
%% https://tools.ietf.org/html/rfc7919).
%%
%% These values were obtained from
%% https://wiki.mozilla.org/Security/Server_Side_TLS#ffdhe2048
%%

dh_params(ffdhe2048) ->
    <<"-----BEGIN DH PARAMETERS-----\n",
      "MIIBCAKCAQEA//////////+t+FRYortKmq/cViAnPTzx2LnFg84tNpWp4TZBFGQz\n",
      "+8yTnc4kmz75fS/jY2MMddj2gbICrsRhetPfHtXV/WVhJDP1H18GbtCFY2VVPe0a\n",
      "87VXE15/V8k1mE8McODmi3fipona8+/och3xWKE2rec1MKzKT0g6eXq8CrGCsyT7\n",
      "YdEIqUuyyOP7uWrat2DX9GgdT0Kj3jlN9K5W7edjcrsZCwenyO4KbXCeAvzhzffi\n",
      "7MA0BM0oNC9hkXL+nOmFg/+OTxIy7vKBg8P+OxtMb61zO7X8vC7CIAXFjvGDfRaD\n",
      "ssbzSibBsu/6iGtCOGEoXJf//////////wIBAg==\n",
      "-----END DH PARAMETERS-----">>;
dh_params(ffdhe3072) ->
    <<"-----BEGIN DH PARAMETERS-----\n",
      "MIIBiAKCAYEA//////////+t+FRYortKmq/cViAnPTzx2LnFg84tNpWp4TZBFGQz\n",
      "+8yTnc4kmz75fS/jY2MMddj2gbICrsRhetPfHtXV/WVhJDP1H18GbtCFY2VVPe0a\n",
      "87VXE15/V8k1mE8McODmi3fipona8+/och3xWKE2rec1MKzKT0g6eXq8CrGCsyT7\n",
      "YdEIqUuyyOP7uWrat2DX9GgdT0Kj3jlN9K5W7edjcrsZCwenyO4KbXCeAvzhzffi\n",
      "7MA0BM0oNC9hkXL+nOmFg/+OTxIy7vKBg8P+OxtMb61zO7X8vC7CIAXFjvGDfRaD\n",
      "ssbzSibBsu/6iGtCOGEfz9zeNVs7ZRkDW7w09N75nAI4YbRvydbmyQd62R0mkff3\n",
      "7lmMsPrBhtkcrv4TCYUTknC0EwyTvEN5RPT9RFLi103TZPLiHnH1S/9croKrnJ32\n",
      "nuhtK8UiNjoNq8Uhl5sN6todv5pC1cRITgq80Gv6U93vPBsg7j/VnXwl5B0rZsYu\n",
      "N///////////AgEC\n",
      "-----END DH PARAMETERS-----">>;
dh_params(ffdhe4096) ->
    <<"-----BEGIN DH PARAMETERS-----\n",
    "MIICCAKCAgEA//////////+t+FRYortKmq/cViAnPTzx2LnFg84tNpWp4TZBFGQz\n",
    "+8yTnc4kmz75fS/jY2MMddj2gbICrsRhetPfHtXV/WVhJDP1H18GbtCFY2VVPe0a\n",
    "87VXE15/V8k1mE8McODmi3fipona8+/och3xWKE2rec1MKzKT0g6eXq8CrGCsyT7\n",
    "YdEIqUuyyOP7uWrat2DX9GgdT0Kj3jlN9K5W7edjcrsZCwenyO4KbXCeAvzhzffi\n",
    "7MA0BM0oNC9hkXL+nOmFg/+OTxIy7vKBg8P+OxtMb61zO7X8vC7CIAXFjvGDfRaD\n",
    "ssbzSibBsu/6iGtCOGEfz9zeNVs7ZRkDW7w09N75nAI4YbRvydbmyQd62R0mkff3\n",
    "7lmMsPrBhtkcrv4TCYUTknC0EwyTvEN5RPT9RFLi103TZPLiHnH1S/9croKrnJ32\n",
    "nuhtK8UiNjoNq8Uhl5sN6todv5pC1cRITgq80Gv6U93vPBsg7j/VnXwl5B0rZp4e\n",
    "8W5vUsMWTfT7eTDp5OWIV7asfV9C1p9tGHdjzx1VA0AEh/VbpX4xzHpxNciG77Qx\n",
    "iu1qHgEtnmgyqQdgCpGBMMRtx3j5ca0AOAkpmaMzy4t6Gh25PXFAADwqTs6p+Y0K\n",
    "zAqCkc3OyX3Pjsm1Wn+IpGtNtahR9EGC4caKAH5eZV9q//////////8CAQI=\n",
    "-----END DH PARAMETERS-----">>.
