%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Parse templates / erlang files in modules, extract all translations.

%% Copyright 2010-2017 Marc Worrell
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

-module(translation_scan).
-author("Marc Worrell <marc@worrell.nl>").

-export([scan/1, scan_file/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


-export([parse_erl/2]).

-spec scan([{App :: atom(), Dir :: file:filename_all()}]) ->
        [{App :: atom(), Dir :: file:filename_all(), Labels :: list()}].
scan(Apps) ->
    [scan_module(App) || App <- Apps].

scan_module({App, Dir}) ->
    Scanned = lists:map(
        fun(File) ->
            Ext = z_convert:to_list(filename:extension(File)),
            scan_file(Ext, File)
        end,
        collect_all_files(Dir)),
    {App, Dir, dedupl(lists:flatten(Scanned))}.

collect_all_files(Dir) ->
    z_utils:wildcard_recursive("*.erl", filename:join(Dir, "src"))
    ++ z_utils:wildcard_recursive("*.tpl", filename:join([Dir, "priv", "templates"])).

dedupl(Trans) ->
    Dict = dict:new(),
    List = dict:to_list(insert(Trans, Dict)),
    [ Tr || {_Key,Tr} <- List ].

insert([], Dict) ->
    Dict;

insert([{Text, Args, Loc}|Trans], Dict) when not is_binary(Text) ->
    insert([{unicode:characters_to_binary(Text), Args, Loc}|Trans], Dict);
insert([{Text, Args, Loc}|Trans], Dict) ->
    case dict:find(Text, Dict) of
        {ok, {_Text, Args0, Loc0}} ->
            Dict1 = dict:store(Text, {Text, merge_args(Args,Args0), [Loc|Loc0]}, Dict),
            insert(Trans, Dict1);
        error ->
            Dict1 = dict:store(Text, {Text, Args, [Loc]}, Dict),
            insert(Trans, Dict1)
    end.

merge_args([], Args) -> Args;
merge_args(Args, []) -> Args;
merge_args([{Lang,Text}|Rest], Args) ->
    case proplists:get_value(Lang, Args) of
        undefined -> merge_args(Rest, [{Lang,Text}|Args]);
        _ -> merge_args(Rest, Args)
    end.

%% @doc Parse the template or Erlang module. Extract all translation tags.
scan_file(".tpl", File) ->
    case template_compiler:translations(File) of
        {ok, Translations} ->
            normalize_line_info(Translations);
        {error, Reason} ->
          lager:error("POT generation, erlang error in \"~s\": ~p~n", [File, Reason])
    end;
scan_file(".erl", File) ->
    IncludeDirs = case zotonic_filehandler:compile_options(File) of
        {ok, Options} -> proplists:get_all_values(i, Options);
        false -> []
    end,
    case epp:open(z_convert:to_list(File), [z_utils:lib_dir(include) | IncludeDirs]) of
        {ok, Epp} ->
            parse_erl(File, Epp);
        {error, Reason} ->
            lager:error("POT generation, erlang error in \"~s\": ~p~n", [File, Reason]),
            []
    end;
scan_file(_, _) ->
    [].

normalize_line_info(Translations) ->
    normalize_line_info(Translations, []).

normalize_line_info([], Acc) ->
    lists:reverse(Acc);
normalize_line_info([{Text, Args, {Filename, LineNr, _ColumnNr}}|Translations], Acc) ->
    normalize_line_info(Translations, [{Text, Args, {Filename, LineNr}}|Acc]).

%% Scan binary for erlang ?__(..., Context) syntax with either a binary or a string as first arg.
parse_erl(File, Epp) ->
    parse_erl_form(epp:parse_erl_form(Epp), File, Epp, []).

parse_erl_form({eof, _}, _File, Epp, Acc) ->
    epp:close(Epp),
    Acc;
parse_erl_form({ok, Other}, File, Epp, Acc) ->
    parse_erl_form(epp:parse_erl_form(Epp), File, Epp,
                   parse_erl_form_part(Other, File, Acc));
parse_erl_form({error, _}, File, Epp, Acc) ->
    parse_erl_form(epp:parse_erl_form(Epp), File, Epp, Acc).

parse_erl_form_part({function, _, _, _, Expressions}, File, Acc) ->
    lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Expressions);
parse_erl_form_part({tuple, _, Expressions}, File, Acc) ->
    lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Expressions);
parse_erl_form_part({clause, _, _, _, Expressions}, File, Acc) ->
    lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Expressions);
parse_erl_form_part({match, _, X, Y}, File, Acc) ->
    parse_erl_form_part(X, File, []) ++ parse_erl_form_part(Y, File, []) ++ Acc;
parse_erl_form_part({cons, _, X, Y}, File, Acc) ->
    parse_erl_form_part(X, File, []) ++ parse_erl_form_part(Y, File, []) ++ Acc;
parse_erl_form_part({op, _, '++', X, Y}, File, Acc) ->
    parse_erl_form_part(X, File, []) ++ parse_erl_form_part(Y, File, []) ++ Acc;
parse_erl_form_part({'case', _, Expr, Exprs}, File, Acc) ->
    parse_erl_form_part(Expr, File, []) ++
        lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Exprs);

parse_erl_form_part({call, _, {remote, _, {atom, _, z_trans}, {atom, _, trans}},
                     [{string, Line, S}, _]}, File, Acc) ->
    [{unicode:characters_to_binary(S), [], {File,Line}}|Acc];
parse_erl_form_part({call, _, {remote, _, {atom, _, z_trans}, {atom, _, trans}},
                     [{bin, _, [{bin_element, _, {string, Line, S}, _, _}|_]}|_]}, File, Acc) ->
    [{unicode:characters_to_binary(S), [], {File,Line}}|Acc];

parse_erl_form_part({call, _, _, Expressions}, File, Acc) ->
    lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Expressions);
parse_erl_form_part({record, _, _, Fields}, File, Acc) ->
    lists:foldl(fun({record_field, _, _, Part}, A) ->
            parse_erl_form_part(Part, File, A)
    end, Acc, Fields);
parse_erl_form_part(_Part, _File, Acc) ->
    Acc. %% ignore
