% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010,2011 Marc Worrell
%% Date: 2010-05-19
%% @doc Parse templates / erlang files in modules, extract all translations.

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

-module(translation_scan).
-author("Marc Worrell <marc@worrell.nl>").

-export([scan/1, scan_file/2]).

-include("zotonic.hrl").

scan(Context) ->
    ModTemplates = [{Path, Files} || {_M, {Path, Files}} <- z_module_indexer:all(template, Context)],
    ModErlang = [{Path, Files} || {_M, {Path, Files}} <- z_module_indexer:all(erlang, Context)],
    Combined = lists:foldl(
                 fun({Path, Files}, Acc) ->
                         [{Path, Files ++ proplists:get_value(Path, ModTemplates, [])}
                          |proplists:delete(Path, Acc)]
                 end, [], ModErlang),
    [ scan_module(Mod) || Mod <- Combined ].

scan_module({Path, Templates}) ->
    {Path, dedupl(lists:flatten([ scan_file(filename:extension(File), File) || {_BaseName,File} <- Templates ]))}.


dedupl(Trans) ->
    Dict = dict:new(),
    List = dict:to_list(insert(Trans, Dict)),
    [ Tr || {_Key,Tr} <- List ].
    
    insert([], Dict) ->
        Dict;
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


%% @doc Parse the Erlang module. Extract all translation tags.
scan_file(".erl", File) ->
    {ok, Path} = zotonic_app:get_path(),
    case epp:open(File, [filename:join(Path, "include")]) of
        {ok, Epp} ->
            parse_erl(File, Epp);
        {error, Reason} ->
            ?ERROR("POT generation, erlang error in ~p: ~p~n", [File, Reason]),
            []
    end;

%% @doc Parse the template in the file. Extract all translation tags.
scan_file(".tpl", File) ->
    case parse(File) of
        {ok, ParseTree} ->
            extract(ParseTree, [], File);
        {error, Reason} ->
            ?ERROR("POT generation, template error in ~p: ~p~n", [File, Reason]),
            []
    end.


parse(File) when is_list(File) ->  
    case catch file:read_file(File) of
        {ok, Data} ->
            case parse(Data) of
                {ok, Val} ->
                    {ok, Val};
                Err ->
                    Err
            end;
        Error ->
            {error, io_lib:format("reading ~p failed (~p)", [File, Error])}  
    end;
parse(Data) when is_binary(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.
    

%% @doc Extract all translation tags from the parse tree.
extract(ParseTree, Acc, F) when is_list(ParseTree) ->
    lists:foldl(fun(Tree,A) -> extract(Tree, A, F) end, Acc, ParseTree);
extract({trans, {trans_text, {_File, Line,_Col}, Text}}, Acc, F) ->
    [{z_string:trim(Text), [], {F,Line}}|Acc];
extract({trans_literal, {_File, Line,_Col}, Text}, Acc, F) ->
    [{Text, [], {F,Line}}|Acc];
extract({trans_ext, {string_literal, {_File, Line,_Col}, Text}, Args}, Acc, F) ->
    [{Text, trans_ext_args(Args,[]), {F,Line}}|Acc];
extract({text, _, _}, Acc, _F) -> Acc;
extract({string_literal, _, _}, Acc, _F) -> Acc;
extract({number_literal, _, _}, Acc, _F) -> Acc;
extract({comment, _}, Acc, _F) -> Acc;
extract({auto_id, _}, Acc, _F) -> Acc;
extract({variable, _}, Acc, _F) -> Acc;
extract(T, Acc, F) when is_tuple(T) ->
    extract(tl(tuple_to_list(T)), Acc, F);
extract(N, Acc, _F) when is_integer(N); is_atom(N) ->
    Acc.

    trans_ext_args([], Acc) ->
        Acc;
    trans_ext_args([{{identifier,_,Lang}, {string_literal, _, Text}}|Args], Acc) ->
        trans_ext_args(Args, [{list_to_atom(Lang), Text}|Acc]).


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
parse_erl_form_part({'case', _, Expr, Exprs}, File, Acc) ->
    parse_erl_form_part(Expr, File, []) ++ 
        lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Exprs);

parse_erl_form_part({call, _, {remote, _, {atom, _, z_trans}, {atom, _, trans}},
                     [{string, Line, S}, _]}, File, Acc) ->
    [{S, [], {File,Line}}|Acc];
parse_erl_form_part({call, _, {remote, _, {atom, _, z_trans}, {atom, _, trans}},
                     [{bin, _, [{bin_element, _, {string, Line, S}, _, _}|_]}|_]}, File, Acc) ->
    [{S, [], {File,Line}}|Acc];

parse_erl_form_part({call, _, _, Expressions}, File, Acc) ->
    lists:foldl(fun(Part,A) -> parse_erl_form_part(Part, File, A) end, Acc, Expressions);
parse_erl_form_part(_Part, _File, Acc) ->
    Acc. %% ignore
