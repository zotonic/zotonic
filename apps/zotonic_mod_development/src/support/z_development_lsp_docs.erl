%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Retrieve module and template-tag documentation for language server hovers.
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

-module(z_development_lsp_docs).
-export([module/1, tag/1, main_tag/1, clean/1]).

%% @doc Read Markdown module documentation, with a source fallback for OTP 26 builds.
-spec module(module()) -> binary().
module(Module) ->
    case code:get_doc(Module) of
        {ok, {docs_v1, _, erlang, <<"text/markdown">>, #{<<"en">> := Text}, _, _}} -> clean(Text);
        {ok, {docs_v1, _, _, _, hidden, _, _}} -> <<>>;
        _ -> source_doc(Module)
    end.

source_doc(Module) ->
    case z_development_editor:source_location(Module) of
        {ok, File, _} ->
            case epp:parse_file(File, [], []) of
                {ok, Forms} ->
                    case [Text || {attribute, _, moduledoc, Text} <- Forms,
                                  is_binary(Text) orelse is_list(Text)]
                    of
                        [Text | _] -> clean(unicode:characters_to_binary(Text));
                        [] -> <<>>
                    end;
                _ -> <<>>
            end;
        _ -> <<>>
    end.

%% @doc Read only named tag manuals beneath the repository documentation directory.
-spec tag(binary()) -> binary().
tag(Tag) ->
    Name = tag_manual(main_tag(Tag)),
    case re:run(Name, <<"^[a-z][a-z0-9_]*$">>, [{capture, none}]) of
        match ->
            Path = filename:join([z_path:get_path(), "doc", "template-tags", binary_to_list(Name) ++ ".md"]),
            case file:read_file(Path) of
                {ok, Text} -> clean(Text);
                _ -> <<>>
            end;
        nomatch ->
            <<>>
    end.

%% @doc The trans tag manual is distinct from the {_ ... _} translation syntax.
tag_manual(<<"trans">>) -> <<"trans_ext">>;
tag_manual(Name) -> Name.

%% @doc Branches and closing tags share their opening tag's manual.
-spec main_tag(binary()) -> binary().
main_tag(<<"else">>) -> <<"if">>;
main_tag(<<"elif">>) -> <<"if">>;
main_tag(<<"empty">>) -> <<"for">>;
main_tag(<<"end", Name/binary>>) -> Name;
main_tag(Name) -> Name.

%% @doc Remove publication metadata and directive wrappers for ordinary Markdown clients.
-spec clean(binary()) -> binary().
clean(Text) ->
    Body = re:replace(Text, <<"\\A---\\r?\\n.*?\\r?\\n---\\r?\\n">>, <<>>, [dotall, {return, binary}]),
    iolist_to_binary(re:replace(Body, <<"^:::[^\\r\\n]*$">>, <<>>, [multiline, global, {return, binary}])).
