%% @author Marc Worrell <marc@worrell.nl>
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%%
%% @copyright 2012 - 2016 Marc Worrell, Maas-Maarten Zeeman
%%
%% @doc SSL self signed ssl certificate management for Zotonic.

%% Copyright 2012 - 2016 Marc Worrell, Maas-Maarten Zeeman
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

-module(mod_ssl_self_signed).

-mod_title("HTTPS / SSL Self Signed Certificates").
-mod_description("Provides simple diy https/ssl certificate management.").
-mod_provides([ssl_certificates]).
-mod_prio(1000).
-mod_depends([ssl]).

-author('Marc Worrell <marc@worrell.nl>').
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-export([
    observe_module_activate/2,
    observe_ssl_options/2,

    cert_files/1,
    check_certs/1,
    dh_params/1
]).

-include_lib("zotonic.hrl").

%% @doc Check the certificates.
%%
observe_module_activate(#module_activate{module=?MODULE}, Context) ->
    case check_certs(Context) of
        {ok, _Certs} ->
            lager:info("SSL: Certificates ready.");
        {error, Reason} ->
            lager:warning("SSL: Problem with certificates: ~p", [Reason])
    end;
observe_module_activate(#module_activate{}, _Context) ->
    ok.

%% @doc Return the certificates of this site.
observe_ssl_options(#ssl_options{}, Context) ->
    {ok, CertFiles} = cert_files(Context),
    CertFiles.

%% @doc Check if all certificates are available in the site's ssl directory
check_certs(Context) ->
	{ok, Certs} = cert_files(Context),
        CertFile = proplists:get_value(certfile, Certs),
        KeyFile = proplists:get_value(keyfile, Certs),
        case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
            {false, false} -> generate_self_signed(Certs, Context);
            {false, true} -> {error, {missing_certfile, CertFile}};
            {true, false} -> {error, {missing_pemfile, KeyFile}};
            {true, true} ->
                case check_keyfile(KeyFile) of
                    ok ->
                        case check_dhfile(proplists:get_value(dhfile, Certs),
                                          m_config:get_value(site, default_dh_param_name, ffdhe3072, Context)) of
                            ok -> {ok, Certs};
                            {error, _}=E -> E
                        end;
                    {error, _}=E -> E
                end
	end.

check_keyfile(Filename) ->
	case file:read_file(Filename) of
		{ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
			{error, {need_rsa_private_key, Filename, "use: openssl rsa -in sitename.key -out sitename.pem"}};
		{ok, Bin} ->
			case public_key:pem_decode(Bin) of
				[] -> {error, {no_private_keys_found, Filename}};
				_ -> ok
			end;
		Error ->
			{error, {cannot_read_pemfile, Filename, Error}}
	end.

check_dhfile(Filename, Default) ->
    case filelib:is_file(Filename) of
        true ->
            case is_dhfile(Filename) of
                true -> ok;
                false -> {error, {no_dhfile, Filename}}
            end;
        false ->
            FileData = dh_params(Default),
            write_dhfile(Filename, FileData)
    end.

is_dhfile(Filename) ->
    case file:read_file(Filename) of
        {ok, <<"-----BEGIN DH PARAMETERS", _/binary>>} -> true;
        _ -> false
    end.

generate_self_signed(Opts, Context) ->
	PemFile = proplists:get_value(keyfile, Opts),
	case filelib:ensure_dir(PemFile) of
		ok ->
			KeyFile = filename:rootname(PemFile) ++ ".key",
			CertFile = proplists:get_value(certfile, Opts),
			Hostname = z_context:hostname(Context),
			Command = "openssl req -x509 -nodes"
					++ " -days 3650"
					++ " -subj '/CN="++Hostname++"'"
					++ " -newkey rsa:2048 "
					++ " -keyout "++KeyFile
					++ " -out "++CertFile,
			lager:info("SSL: ~p", [Command]),
			Result = os:cmd(Command),
			lager:info("SSL: ~p", [Result]),
			case file:read_file(KeyFile) of
				{ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
					os:cmd("openssl rsa -in "++KeyFile++" -out "++PemFile),
					{ok, Opts};
				{ok, <<"-----BEGIN RSA PRIVATE KEY", _/binary>>} ->
					file:rename(KeyFile, PemFile),
					{ok, Opts};
				_Error ->
					{error, "Could not generate self signed certificate"}
			end;
		{error, _} = Error ->
			{error, {ensure_dir, Error, PemFile}}
	end.

write_dhfile(Filename, Data) ->
    case filelib:ensure_dir(Filename) of
        ok ->
            case file:write_file(Filename, Data) of
                ok -> ok;
                {error, Reason} ->
                    {error, {write_dhfile, Reason, Filename}}
            end;
        {error, _} = Error ->
            {error, {ensure_dir, Error, Filename}}
    end.

cert_files(Context) ->
	SSLDir = filename:join(z_path:site_dir(Context), "ssl"),
	Sitename = z_convert:to_list(z_context:site(Context)),
	Files = [
		{certfile, filename:join(SSLDir, Sitename++".crt")},
		{keyfile, filename:join(SSLDir, Sitename++".pem")},
		{password, m_config:get_value(mod_ssl, password, "", Context)},
                {dhfile, filename:join(SSLDir, "dh-params.pem")}
	],
	CaCertFile = filename:join(SSLDir, Sitename++".ca.crt"),
	case filelib:is_file(CaCertFile) of
		false -> {ok, Files};
		true -> {ok, [{cacertfile, CaCertFile} | Files]}
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
      "-----END DH PARAMETERS-----\n">>;
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
      "-----END DH PARAMETERS-----\n">>;
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
    "-----END DH PARAMETERS-----\n">>.
