%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Map editor and container source paths so LSP navigation opens files on the editor host.
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

-module(z_development_lsp_paths).

-moduledoc(<<"
Optional host/container path translation for stdio LSP sessions. Empty mappings preserve the local
fast path. Roots are matched by path components, never by raw string prefixes.
">>).

-export([prepare/1, to_server/2, definitions/2]).

%% @doc Validate mappings once per session and prefer the most specific mount in each direction.
-spec prepare(list()) -> [] | map().
prepare([]) -> [];
prepare(Mappings) when is_list(Mappings), length(Mappings) =< 32 ->
    Pairs = [{root(Local), root(filter_development_filename:resolve(filename:join(root(Remote))))}
        || #{<<"localRoot">> := Local, <<"remoteRoot">> := Remote} <- Mappings],
    true = length(Pairs) =:= length(Mappings),
    true = length(lists:usort([L || {L, _} <- Pairs])) =:= length(Pairs),
    true = length(lists:usort([R || {_, R} <- Pairs])) =:= length(Pairs),
    #{to_server => ordered(Pairs), to_client => ordered([{R, L} || {L, R} <- Pairs])}.

%% @doc Translate an editor file to the container; never resolve host symlinks on the server.
-spec to_server(binary(), [] | map()) -> binary().
to_server(File, []) -> File;
to_server(File, #{to_server := Mappings}) -> translate(File, Mappings).

%% @doc Translate definition URIs back to the editor without touching the host filesystem.
-spec definitions(term(), [] | map()) -> term().
definitions(Result, []) -> Result;
definitions(Locations, #{to_client := Mappings}) when is_list(Locations) ->
    [
        Location#{uri => encode(translate(z_development_lsp_text:path(Uri), Mappings))}
        || #{uri := Uri} = Location <- Locations
    ];
definitions(Result, _) -> Result.

root(Path) when is_binary(Path), byte_size(Path) > 0, byte_size(Path) < 4096 ->
    Path = unicode:characters_to_binary(Path),
    absolute = filename:pathtype(Path),
    nomatch = binary:match(Path, <<0>>),
    %% Reject traversal even if filename:split/1 would normalize it away.
    false = lists:any(fun(P) -> P =:= <<".">> orelse P =:= <<"..">> end,
                      binary:split(Path, <<"/">>, [global])),
    filename:split(Path).

ordered(Pairs) ->
    lists:sort(fun({A, _}, {B, _}) -> length(A) > length(B) end, Pairs).

translate(File, Mappings) ->
    Parts = root(File),
    case [{From, To} || {From, To} <- Mappings, lists:prefix(From, Parts)] of
        [{From, To} | _] -> filename:join(To ++ lists:nthtail(length(From), Parts));
        [] -> throw({lsp_error, <<"No LSP path mapping for: ", File/binary>>})
    end.

encode(Path) ->
    Encoded = uri_string:quote(Path, "/:"),
    <<"file://", Encoded/binary>>.
