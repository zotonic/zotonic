%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc Decrypt encrypted backup files.
%% @end

%% Copyright 2024 Maas-Maarten Zeeman
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

-module(zotonic_cmd_decrypt).

%% API
-export([info/0, run/1]).

info() ->
    "Decrypt an encrypted backup.".

run([ Password, InFilename]) ->
    decrypt(InFilename, Password);
run([ Password, InFilename, OutFilename]) ->
    decrypt(InFilename, OutFilename, Password);
run(_) ->
    io:format("USAGE: decrypt password in_filename [out_filename]~n"),
    halt(1).

decrypt(InFilename, Password) ->
    feedback(mod_backup_file_crypto:password_decrypt(InFilename, z_convert:to_binary(Password))).

decrypt(InFilename, OutFilename, Password) ->
    feedback(mod_backup_file_crypto:password_decrypt(InFilename, OutFilename, z_convert:to_binary(Password))).

feedback({ok, _OutFile}) ->
    ok;
feedback({error, wrong_password}) ->
    io:format("ERROR: Wrong password.~n~nNote: The backup encryption password can be found in Admin -> System -> Config.");
feedback({error, Error}) ->
    io:format("ERROR: Sorry, something went wrong.~n~nDescription: ~p.", [Error]).
