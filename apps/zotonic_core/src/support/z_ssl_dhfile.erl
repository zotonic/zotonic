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

%% @doc Check if the given file is a DH file.
is_dhfile(Filename) ->
    zotonic_ssl_dhfile:is_dhfile(Filename).

%% @doc Ensure that a DH file is generated. The 'ssl_dhgroup' config defines
%% the group for the DH file, defaults to 'ffdhe3072'
-spec ensure_dhfile() -> ok | {error, term()}.
ensure_dhfile() ->
    Filename = dhfile(),
    case filelib:is_file(Filename) of
        true ->
            ok;
        false ->
            ok = z_filelib:ensure_dir(Filename),
            Group = z_convert:to_atom(z_config:get(ssl_dhgroup, ?DEFAULT_DHGROUP)),
            zotonic_ssl_dhfile:ensure_dhfile(dhfile(), Group)
    end.

%% @doc Return the filename of the DH file. Defaults to a file in the security
%% directory. Can be configured differently using the 'ssl_dhfile' config.
-spec dhfile() -> file:filename_all().
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
