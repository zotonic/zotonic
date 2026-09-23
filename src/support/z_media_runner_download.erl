%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Stream and verify media runner output downloads before publishing local files.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_runner_download).

-export([install/4, fetch/3]).

%% @doc Download and verify every output before replacing any caller-owned file.
-spec install(list(), list(), map(), function()) -> ok.
install(Files, Paths, Options, Fetch) ->
    Pending = [pending_file(F, Paths) || F <- Files],
    try
        lists:foreach(fun({F, _, Temp}) ->
            Size = maps:get(<<"size">>, F),
            Hash = maps:get(<<"sha256">>, F),
            true = is_integer(Size) andalso Size >= 0 andalso Size =< z_media_runner_protocol:output_limit(),
            true = is_binary(Hash) andalso byte_size(Hash) =:= 64,
            match = re:run(Hash, <<"^[0-9a-f]{64}$">>, [{capture, none}]),
            case Fetch(F, Temp, Options) of
                {ok, Size, Hash} -> ok;
                {error, Reason} ->
                    case transport_failure(Reason) of
                        true -> throw({media_runner_download, Reason});
                        false -> error(invalid_download)
                    end;
                _ -> error(invalid_download)
            end
        end, Pending),
        lists:foreach(fun({_, Path, Temp}) -> ok = file:rename(Temp, Path) end, Pending)
    after
        lists:foreach(fun({_, _, Temp}) -> file:delete(Temp) end, Pending)
    end.

pending_file(File, Paths) ->
    Path = proplists:get_value(maps:get(<<"id">>, File), Paths),
    Suffix = z_media_runner_protocol:hex(crypto:strong_rand_bytes(16)),
    {File, Path, <<(unicode:characters_to_binary(Path))/binary, ".download-", Suffix/binary>>}.

%% @doc Stream only from the configured runner; never forward OAuth credentials to callback-supplied hosts.
-spec fetch(map(), file:filename_all(), map()) -> {ok, non_neg_integer(), binary()} | {error, term()}.
fetch(#{<<"url">> := Url, <<"size">> := Size, <<"sha256">> := Hash}, Temp,
        #{media_runner_endpoint := Base, media_runner_token := Token}) ->
    Url = <<Base/binary, "/results/", Hash/binary>>,
    true = z_media_runner_protocol:http_url(Url),
    Headers = [{"authorization", "Bearer " ++ binary_to_list(Token)}],
    z_media_runner_http:download(Url, Headers, Temp, Size, 3600000).

%% Only transport failures are retryable. Invalid headers, sizes and hashes,
%% authentication failures and local file errors must not trigger another render.
transport_failure({failed_connect, _}) -> true;
transport_failure({shutdown, server_closed}) -> true;
transport_failure({tcp_error, _, _}) -> true;
transport_failure({ssl_error, _, _}) -> true;
transport_failure({http_status, Code}) -> lists:member(Code, [404, 429, 502, 503, 504]);
transport_failure(Reason) ->
    lists:member(Reason, [timeout, socket_closed_remotely, closed, econnreset,
        econnrefused, etimedout, enetunreach, ehostunreach, nxdomain]).
