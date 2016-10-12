% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010,2011 Marc Worrell
%% Date: 2010-05-19
%% @doc Generate .po files for all found labels. The .po files are generated per module and language

%% Copyright 2010,2011 Marc Worrell
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

-module(translation_po).
-author("Marc Worrell <marc@worrell.nl>").

-export([generate/2]).

-include_lib("zotonic.hrl").


%% @doc Take the list of found labels per module and generate all po files for those labels in the module directories.
-spec generate([{ModuleName :: atom(), Labels :: list()}], #context{}) -> ok.
generate(ModLabels, Context) ->
    generate1(ModLabels, Context).

-spec generate1([{atom(), list()}], #context{}) -> ok.
generate1([], _Context) ->
    ok;
generate1([{Module, Labels}|ModuleLabels], Context) ->
    Path = template_path(Module, Context),
    Dir = filename:dirname(Path),

    case filelib:ensure_dir(filename:join([Dir, "empty"])) of
        ok ->
            delete_po_files(Dir),
            generate_po_files(Module, Dir, Labels),
            generate1(ModuleLabels, Context);
        {error, Reason} ->
            lager:warning("Could not create directory for extracted translations: ~p ~p", [{error, Reason}, Dir]),
            generate1(ModuleLabels, Context)
    end.

%% Delete all existing po files in a directory
delete_po_files(Dir) ->
    Files = z_utils:wildcard(filename:join(Dir, "[a-z][a-z].{po,pot}")),
    [ file:delete(F) || F <- Files ].

%% Generate po files for all languages found in the labels.
generate_po_files(_ModuleName, _Dir, []) ->
    ok;
generate_po_files(ModuleName, Dir, Labels) ->
    Languages = extract_languages(Labels, [en]),
    [ generate_po_file(ModuleName, Lang, Dir, Labels) || Lang <- Languages ].


extract_languages([], Acc) ->
    Acc;
extract_languages([{_Text, Args, _Pos}|Labels], Acc) ->
    Acc1 = lists:foldl(fun ensure_lang/2, Acc, Args),
    extract_languages(Labels, Acc1).

    ensure_lang({Lang,_}, List) ->
        case lists:member(Lang, List) of
            true -> List;
            false -> [Lang|List]
        end.


generate_po_file(ModuleName, Lang, Dir, Labels) ->
    Filename = filename:join([Dir, z_convert:to_list(ModuleName) ++ case Lang of en -> ".pot"; _ -> ".po" end]),
    PoLabels = extract_labels(Lang, Labels, []),
    z_gettext_compile:generate(Filename, lists:sort(PoLabels)).


extract_labels(_Lang, [], Acc) ->
    Acc;
extract_labels(en, [{Lab, _Trans, Pos}|Labels], Acc) ->
    extract_labels(en, Labels, [{Lab, "", Pos}|Acc]);
extract_labels(Lang, [{Lab, Trans, Pos}|Labels], Acc) ->
    extract_labels(Lang, Labels, [{Lab, proplists:get_value(Lang, Trans, Lab), Pos}|Acc]).


%% @doc Get the path to a module's translation template (.pot) file
-spec template_path(atom(), #context{}) -> string().
template_path(Module, Context) ->
    CoreModules = z_module_manager:scan_core(Context),
    AllModules = z_module_manager:scan(Context),
    TemplateParentDir = case proplists:lookup(Module, CoreModules) of
        none ->
            %% Custom module
            proplists:get_value(Module, AllModules);
        {_Module, _Path} ->
            %% Core module
            z_utils:lib_dir(priv)
    end,
    filename:join([TemplateParentDir, "translations", "template", z_convert:to_list(Module) ++ ".pot"]).
