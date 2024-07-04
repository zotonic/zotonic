%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc Decrypt and encrypt files.
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

-module(mod_backup_file_crypto).

-export([
    password_encrypt/2, password_encrypt/3,
    password_decrypt/2, password_decrypt/3
]).

-define(DEFAULT_BLOCK_SIZE, 100000).
-define(MIN_ITER, 600000).
-define(MAX_ITER, 20000000).
-define(SALT_SIZE, 16).
-define(IV_SIZE, 16).
-define(KEY_SIZE, 32).

% @doc Encrypt file named Filename with a password. The output will be written to the file named InFile + ".enc".
password_encrypt(Filename, Password) ->
    password_encrypt(Filename, <<Filename/binary, ".enc">>, Password).

% @doc Decrypt the file name Filename with the given password. When the extension of the file is ".enc"
% the decrypted content will be written to the file without that extenstion.
password_decrypt(Filename, Password) ->
    case filename:extension(Filename) of
        DotEnc when DotEnc == <<".enc">> orelse DotEnc == ".enc" ->
            OutFilename = filename:rootname(Filename),
            password_decrypt(Filename, OutFilename, Password);
        _ ->
            {error, no_outfile}
    end.


% @doc Encrypt file named InFile with a password. The encrypted output will be written to OutFile.
password_encrypt(InFile, OutFile, Password) ->
    with_read_write_files(fun(In, Out) ->
                                  case password_encrypt_stream(In, Out, Password) of
                                      ok ->
                                          {ok, OutFile};
                                      {error, _}=Error ->
                                          Error
                                  end
                          end,
                          InFile, OutFile).

% @doc Decrypt the file name InFile with Password and write the output to OutFile.
password_decrypt(InFile, OutFile, Password) -> 
    with_read_write_files(fun(In, Out) ->
                                  case password_decrypt_stream(In, Out, Password) of
                                      ok ->
                                          {ok, OutFile};
                                      {error, _}=Error ->
                                          Error
                                  end
                          end,
                          InFile, OutFile).




%%
%% Helpers
%%

password_encrypt_stream(InIODevice, OutIODevice, Password) ->
    password_encrypt_stream(InIODevice, OutIODevice, Password, new_salt(), new_iter()).

password_encrypt_stream(InIODevice, OutIODevice, Password, Salt, Iter) ->
    %% Write the non-secret salt and extra iteration count, they are not
    %% secret. It is handy to store these parameters together with the
    %% encrypted data.
    ok = file:write(OutIODevice, make_header(Salt, Iter)),

    #{ key := Key, iv := IV } = derive_key_and_iv(Password, Salt, Iter),
    CipherState = crypto:crypto_init(aes_256_cfb8, Key, IV, [{encrypt, true}, {padding, random}]),
    write_encrypted_mac(password_mac(Salt, Password), OutIODevice, CipherState),
    stream_crypto(InIODevice, OutIODevice, CipherState).

password_decrypt_stream(InIODevice, OutIODevice, Password) ->
    {ok, Header} = file:read(InIODevice, get_header_size()),
    case get_decrypt_params(Header) of
        {ok, #{ alg := Alg, iter := Iter, salt := Salt}} ->
            #{ key := Key, iv := IV } = derive_key_and_iv(Password, Salt, Iter),
            CipherState = crypto:crypto_init(Alg, Key, IV, [{encrypt, false}, {padding, random}]),
            case check_mac(password_mac(Salt, Password), InIODevice, CipherState) of
                ok ->
                    stream_crypto(InIODevice, OutIODevice, CipherState);
                {error, _}=Error ->
                    Error
            end;
        {error, _}=Error ->
            Error
    end.


% Create a mac of the salt and the password. Used to check the password before
% decrypting the whole file.
password_mac(Salt, Password) ->
    crypto:mac(hmac, sha256, Password, <<Salt/binary, Password/binary>>).

% Write the encrypted mac to the output device.
write_encrypted_mac(Mac, Out, CipherState) ->
    ok = file:write(Out, crypto:crypto_update(CipherState, Mac)).

% Check the password mac, when it is wrong, the user used the wrong
% password.
check_mac(Mac, In, CipherState) ->
    case file:read(In, size(Mac)) of
        {ok, Data} ->
            case crypto:crypto_update(CipherState, Data) of
                Mac -> ok;
                _ -> {error, wrong_password}
            end;
        eof ->
            {error, eof};
        {error, _}=Error ->
            Error
    end.

% Block encrypt or decrypt the passed io_device
stream_crypto(In, Out, CipherState) ->
    stream_crypto(In, Out, CipherState, ?DEFAULT_BLOCK_SIZE).

stream_crypto(In, Out, CipherState, BlockSize) ->
    case file:read(In, BlockSize) of
        {ok, Data} ->
            ok = file:write(Out, crypto:crypto_update(CipherState, Data)),
            stream_crypto(In, Out, CipherState, BlockSize);
        eof ->
            ok = file:write(Out, crypto:crypto_final(CipherState));
        {error, _}=Error ->
            Error
    end.

new_salt() ->
    crypto:strong_rand_bytes(?SALT_SIZE).

new_iter() ->
    ?MIN_ITER + rand:uniform(65535).

derive_key_and_iv(Password, Salt, Iter) ->
    <<IV:?IV_SIZE/binary, Key:?KEY_SIZE/binary>> = crypto:pbkdf2_hmac(sha256, Password, Salt, Iter, ?IV_SIZE + ?KEY_SIZE),
    #{ key => Key, iv => IV }.

% Get the size of the prefix of the encrypted files. Needs to corresponde with the size in make_header/2
get_header_size() ->
    2 + 4 + ?SALT_SIZE.

make_header(Salt, Iter) when size(Salt) == ?SALT_SIZE andalso (Iter =< ?MIN_ITER orelse Iter < ?MAX_ITER) ->
    <<"Z1", Iter:32/little-unsigned-integer, Salt/binary>>.

get_decrypt_params(<<"Z1", Iter:32/little-unsigned-integer, Salt:?SALT_SIZE/binary, _/binary>>)
  when Iter < ?MAX_ITER ->
    Params = #{ alg => aes_256_cfb8, iter => Iter, salt => Salt},
    {ok, Params};
get_decrypt_params(_) ->
    {error, bad_prefix}.

with_read_write_files(Fun, InFile, OutFile) ->
    case file:open(InFile, [read, binary]) of
        {ok, In} ->
            try
                case file:open(OutFile, [write, binary]) of
                    {ok, Out} ->
                        try
                            Fun(In, Out)
                        after
                            file:close(Out)
                        end;
                    {error, OutFileError} ->
                        {error, {outfile, OutFileError}}
                end
            after
                file:close(In)
            end;
        {error, InFileError} ->
            {error, {infile, InFileError}}
    end.


