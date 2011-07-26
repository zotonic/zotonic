% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-19
%% @doc Parse a template, extract all translations.

%% Copyright 2010 Marc Worrell
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

-export([scan/1]).

-include("zotonic.hrl").

scan(Context) ->
    ModTemplates = z_module_indexer:all(template, Context),
    [ scan_module(Mod) || Mod <- ModTemplates ].
    
scan_module({Module, {Path, Templates}}) ->
    {Module, Path, dedupl(lists:flatten([ scan_file(File) || {_BaseName,File} <- Templates ]))}.


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


%% @doc Parse the template in the file. Extract all translation tags.
scan_file(File) ->
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
