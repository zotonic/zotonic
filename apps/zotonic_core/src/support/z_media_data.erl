%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2020 Arjan Scherpenisse, Marc Worrell
%% @doc Images as data: urls.

%% Copyright 2010-2020 Arjan Scherpenisse, Marc Worrell
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

-module(z_media_data).

-export([
    file_data/2,
    file_data_url/2
    ]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

file_data_url(Path, Context) ->
    case file_data(Path, Context) of
        {ok, {Mime, Data}} ->
            {ok, iolist_to_binary([
                <<"data:">>, Mime,
                <<";base64,">>,
                base64:encode(Data)
            ])};
        {error, _} = Error ->
            Error
    end.

file_data(Path, Context) ->
    case lookup(Path, Context) of
        {ok, #z_file_info{ mime = Mime } = Info} ->
            try
                {ok, {Mime, z_file_request:content_data(Info, identity)}}
            catch
                Type:Err ->
                    lager:warning("File data error for ~p: ~p", [ Path, {Type, Err} ]),
                    {error, data}
            end;
        {error, _} = Error ->
            Error
    end.

lookup(<<"/lib/", Path/binary>>, Context) ->
    SafePath = mochiweb_util:unquote(Path),
    if_visible(lookup_decoded(<<"lib">>, SafePath, Context), Context);
lookup(<<"/image/", Path/binary>>, Context) ->
    SafePath = mochiweb_util:unquote(Path),
    if_visible(lookup_decoded(<<"lib">>, SafePath, Context), Context);
lookup(_, _Context) ->
    <<>>.

if_visible({ok, #z_file_info{} = Info} = OK, Context) ->
    case z_file_request:is_visible(Info, Context) of
        true ->
            OK;
        false ->
            {error, eacces}
    end;
if_visible({error, _} = Error, _Context) ->
    Error.

lookup_decoded(<<"lib">>, Path, Context) ->
    z_file_request:lookup_lib(Path, Context);
lookup_decoded(<<"image">>, Path, Context) ->
    z_file_request:lookup_file(Path, Context).
