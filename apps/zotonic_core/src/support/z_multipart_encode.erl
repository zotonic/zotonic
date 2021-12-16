%% @author Arthur Clemens, Marc Worrell
%% @copyright 2021 Arthur Clemens, Marc Worrell
%% @doc Encode a list of fields and/or files as multipart/form-data

%% Copyright 2021 Arthur Clemens, Marc Worrell
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

-module(z_multipart_encode).

-export([ encode/1 ]).

-define(NL, <<"\r\n">>).

-spec encode(Parts) -> {Body, ContentType} when
    Parts :: list(map()),
    Body :: binary(),
    ContentType :: binary().
encode(Parts) ->
    Boundary = z_ids:id(32),
    EncodedParts = lists:map( fun(P) -> encode_part(P, Boundary) end, Parts ),
    CT = iolist_to_binary([ "multipart/form-data; boundary=", Boundary ]),
    {iolist_to_binary([ EncodedParts, <<"--">>, Boundary, <<"--">>, ?NL ]), CT}.

encode_part(#{ name := Name, value := Value }, Boundary) ->
    [
        <<"--">>, Boundary, ?NL,
        <<"Content-Disposition: form-data; name=\"">>, Name, <<"\"">>, ?NL, ?NL,
        Value, ?NL
    ];
encode_part(#{ name := Name, data := Data } = P, Boundary) ->
    Mime = maps:get(mime, P, <<"application/octet-stream">>),
    Filename = case maps:get(filename, P, undefined) of
        undefined -> [ Name, z_media_identify:extension(Mime) ];
        F -> F
    end,
    [
        <<"--">>, Boundary, ?NL,
        <<"Content-Disposition: form-data; name=\"">>, Name, <<"\"; filename=\"">>, Filename, <<"\"">>, ?NL,
        <<"Content-Type: ">>, Mime, ?NL, ?NL,
        Data,
        ?NL
    ].
