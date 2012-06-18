%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%
%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('marc@worrell.nl').

-include_lib("zotonic.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/3, compile/4, compile/5, parse/1]).

-record(dtl_context, {
    local_scopes = [], 
    block_dict = dict:new(), 
    auto_escape = off, 
    parse_trail = [],
    extends_trail = [],
    block_trail = [],
    vars = [],
    custom_tags_dir = [],
    reader = {file, read_file},
    finder = undefined,
    module = [],
    compiler_options = [verbose, report_errors],
    force_recompile = false,
    z_context = undefined}).

-record(ast_info, {
    dependencies = [],
    var_names = [],
    pre_render_asts = []}).
    
-record(treewalker, {
    counter = 0,
    has_auto_id = false,
    custom_tags = []}).    

compile(Binary, Module, ZContext) when is_binary(Binary) ->
    compile(Binary, Module, [], ZContext);

compile(File, Module, ZContext) ->
    compile(File, File, Module, [], ZContext).

compile(Binary, Module, Options, ZContext) when is_binary(Binary) ->
    compile(Binary, [], Module, Options, ZContext);
    
compile(File, Module, Options, ZContext) ->  
    compile(File, filename:basename(File), Module, Options, ZContext).

compile(Binary, BaseFile, Module, Options, ZContext) when is_binary(Binary) ->
    TemplateResetCounter =  proplists:get_value(template_reset_counter, Options, 0),
    case parse(Binary) of
        {ok, DjangoParseTree} ->
            case compile_to_binary( BaseFile,
                                    DjangoParseTree, 
                                    init_dtl_context(BaseFile, BaseFile, Module, Options, ZContext),
                                    TemplateResetCounter) of
                {ok, Module1, _Bin} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;

compile(File, BaseFile, Module, Options, ZContext) ->  
    Context = init_dtl_context(File, BaseFile, Module, Options, ZContext),
    TemplateResetCounter =  proplists:get_value(template_reset_counter, Options, 0),
    case parse(File, Context) of  
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, Context, TemplateResetCounter) of
                {ok, Module1, _Bin} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        {error, {ErrLoc, ErrModule, ["syntax error before: " = ErrMsg, [$[,Text,$]]]}} when is_list(Text) ->
            Text1 = [ list_to_integer(C) || C <- Text, is_list(C) ],
            {error, {ErrLoc, ErrModule, ErrMsg ++ Text1}};
        {error, {ErrLoc, ErrModule, ErrMsg}} ->
            {error, {ErrLoc, ErrModule, lists:flatten(ErrMsg)}};
        Err ->
            Err
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

compile_to_binary(File, DjangoParseTree, Context, TemplateResetCounter) ->
    try body_ast(DjangoParseTree, Context, #treewalker{}) of
        {{Ast, Info}, TreeWalker} ->
            case compile:forms(forms(File, Context#dtl_context.module, Ast, Info, Context, TreeWalker, TemplateResetCounter), 
                    Context#dtl_context.compiler_options) of
                {ok, Module1, Bin} -> 
                    code:purge(Module1),
                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                        {module, _} -> {ok, Module1, Bin};
                        _ -> {error, lists:concat(["code reload failed: ", Module1])}
                    end;
                error ->
                    {error, lists:concat(["compilation failed: ", File])};
                OtherError ->
                    OtherError
            end
    catch 
        throw:Error -> Error
    end.


init_dtl_context(File, BaseFile, Module, Options, ZContext) when is_list(Module) ->
    init_dtl_context(File, BaseFile, list_to_atom(Module), Options, ZContext);
init_dtl_context(File, BaseFile, Module, Options, ZContext) ->
    Ctx = #dtl_context{},
    #dtl_context{
        local_scopes = [ [{'$autoid', erl_syntax:variable("AutoId_"++z_ids:identifier())}] ],
        parse_trail = [File], 
        extends_trail = [BaseFile],
        module = Module,
        custom_tags_dir = proplists:get_value(custom_tags_dir, Options, Ctx#dtl_context.custom_tags_dir),
        vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        finder = proplists:get_value(finder, Options, Ctx#dtl_context.finder),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#dtl_context.force_recompile),
        z_context = ZContext}.   
    
parse(File, Context) ->  
    {M,F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            case scan_parse(File, Data) of
                {ok, Val} ->
                    {ok, Val};
                Err ->
                    Err
            end;
        Error ->
            {error, io_lib:format("reading ~p failed (~p)", [File, Error])}  
    end.

% Used for parsing tests
parse(Data) when is_binary(Data) ->
    scan_parse("string", Data).

scan_parse(SourceRef, Data) ->
    case erlydtl_scanner:scan(SourceRef, binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        
  
forms(File, Module, BodyAst, BodyInfo, Context, TreeWalker, TemplateResetCounter) ->
    TemplateResetCounterFunctionAst = erl_syntax:function(
        erl_syntax:atom(template_reset_counter),
            [ erl_syntax:clause(
                    [],
                    none,
                    [erl_syntax:integer(TemplateResetCounter)]
                    )
            ]),

    TransTableFunctionAst = erl_syntax:function(
        erl_syntax:atom(trans_table),
            [ erl_syntax:clause(
                    [],
                    none,
                    [erl_syntax:abstract(z_trans_server:table(Context#dtl_context.z_context))]
                    )
            ]),

    Function2 = erl_syntax:application(none, erl_syntax:atom(render2), 
        [erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render2FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  
     
    SourceFunctionAst = erl_syntax:function(
        erl_syntax:atom(source),
            [ erl_syntax:clause([], none, [ erl_syntax:string(File) ]) ]),

    Dependencies  = lists:usort([{File, filelib:last_modified(File)} | BodyInfo#ast_info.dependencies]),
    Dependencies1 = lists:filter(fun({[],0}) -> false; (_) -> true end, Dependencies),
    DependenciesFunctionAst = erl_syntax:function(
        erl_syntax:atom(dependencies), [
                erl_syntax:clause([], none, 
                    [ erl_syntax:list( lists:map(
                            fun ({XFile, {{Year,Month,Day},{Hour,Min,Sec}}}) ->
                                erl_syntax:tuple([
                                    erl_syntax:string(XFile),
                                    erl_syntax:tuple([
                                        erl_syntax:tuple([erl_syntax:integer(Year), erl_syntax:integer(Month), erl_syntax:integer(Day)]),
                                        erl_syntax:tuple([erl_syntax:integer(Hour), erl_syntax:integer(Min), erl_syntax:integer(Sec)])
                                    ])
                                ])
                            end, 
                            Dependencies1)) ])
            ]),     

	BodyLanguageAst = erl_syntax:match_expr(
							erl_syntax:variable("Language"),
							erl_syntax:application(
						        erl_syntax:atom(z_context), 
						        erl_syntax:atom(language),
						        [ z_context_ast(Context) ]
							)
					),

    BodyRenderAsts = case TreeWalker#treewalker.has_auto_id of
        false ->
            [BodyLanguageAst, BodyAst];
        true -> 
            AutoIdVar = resolve_scoped_variable_ast("$autoid", Context),
            BodyAutoIdAst = erl_syntax:match_expr(
                                    AutoIdVar,
                                    erl_syntax:application(
                                                erl_syntax:atom(z_ids),
                                                erl_syntax:atom(identifier),
                                                [erl_syntax:integer(8)]
                                    )
                             ),
            [BodyAutoIdAst, BodyLanguageAst, BodyAst]
    end,

    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render2), 
            [ erl_syntax:clause(
					[erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")], 
					none, 
                	BodyRenderAsts)
			]),   
    
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([
		            erl_syntax:arity_qualifier(erl_syntax:atom(template_reset_counter), erl_syntax:integer(0)),
		            erl_syntax:arity_qualifier(erl_syntax:atom(trans_table), erl_syntax:integer(0)),
					erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0))])]),

    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, TemplateResetCounterFunctionAst, TransTableFunctionAst,
            Render2FunctionAst, SourceFunctionAst, DependenciesFunctionAst, RenderInternalFunctionAst
            | BodyInfo#ast_info.pre_render_asts]].    


find_next([], _Find) -> error;
find_next([Find,Next|_], Find) -> {ok, Next};
find_next([_|Rest], Find) -> find_next(Rest, Find).


% child templates should only consist of blocks at the top level
body_extends(Extends, File, ThisParseTree, Context, TreeWalker) ->
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw({error, "Circular file inclusion: " ++ File});
        _ ->
            notify(#debug{what=template, arg={extends, Extends, File}}, Context#dtl_context.z_context),
            case parse(File, Context) of
                {ok, ParentParseTree} ->
                    ThisFile = hd(Context#dtl_context.parse_trail),
                    BlockDict = lists:foldl(
                        fun
                            ({block, {identifier, _, Name}, Contents}, Dict) ->
                                Dict1 = dict:store(Name, {ThisFile, Contents}, Dict),
                                dict:store({Name, ThisFile}, Contents, Dict1);
                            (_, Dict) ->
                                Dict
                        end, dict:new(), ThisParseTree),
                    with_dependency(File, body_ast(ParentParseTree, Context#dtl_context{
                        block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                                                BlockDict,
                                                Context#dtl_context.block_dict),
                        block_trail = [],
                        parse_trail = [File | Context#dtl_context.parse_trail],
                        extends_trail = [Extends | Context#dtl_context.extends_trail]}, TreeWalker));
                Err ->
                    throw(Err)
            end        
    end.

body_ast([overrules | ThisParseTree], Context, TreeWalker) ->
    CurrentExtend = hd(Context#dtl_context.extends_trail),
    CurrentFile = hd(Context#dtl_context.parse_trail),
    Files = full_path(CurrentExtend, true, Context),
    % Find the first file after the current file
    case find_next(Files, CurrentFile) of
        {ok, File} ->
            notify(#debug{what=template, arg={overrules, CurrentExtend, File}}, Context#dtl_context.z_context),
            body_extends(CurrentExtend, File, ThisParseTree, Context, TreeWalker);
        error ->
            ?ERROR("body_ast: could not find overruled template for \"~p\" (~p)", [CurrentExtend,CurrentFile]),
            throw({error, "Could not find the template for overrules: '" ++ CurrentExtend ++ "'"}),
            {{erl_syntax:string(""), #ast_info{}}, TreeWalker}
    end;
    
body_ast([{extends, {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    Extends = unescape_string_literal(String),
    case full_path(Extends, Context) of
        {ok, File} ->
            body_extends(Extends, File, ThisParseTree, Context, TreeWalker);
       {error, Reason} ->
            ?ERROR("body_ast: could not find template ~p (~p)", [Extends, Reason]),
            throw({error, "Could not find the template for extends: '" ++ Extends ++ "'"}),
            {{erl_syntax:string(""), #ast_info{}}, TreeWalker}
    end;

body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
        fun
            ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                CurrentFile = case Context#dtl_context.block_trail of
                                    [] -> hd(Context#dtl_context.parse_trail);
                                    [{_, F}|_] -> F
                              end,
                % remember this block for an 'inherit' tag
                Context1 = Context#dtl_context{
                    block_dict=dict:store({Name,CurrentFile}, Contents, Context#dtl_context.block_dict)
                },
                % See if this block has been overruled
                {BlockFile, Block} = case dict:find(Name, Context#dtl_context.block_dict) of
                    {ok, {_ChildFile, _ChildBlock} = B} ->
                        B;
                    error ->
                        {CurrentFile, Contents}
                end,
                % Check if we have a recursive definition
                case lists:member({Name,BlockFile}, Context#dtl_context.block_trail) of
                    true ->
                        ?ERROR("body_ast: recursive block ~p (~p)", [Name, BlockFile]),
                        throw({error, "Recursive block definition of '" ++ Name ++ "' (" ++ BlockFile ++ ")"});
                    false ->
                        body_ast(Block,
                            Context1#dtl_context{block_trail=[{Name,BlockFile}|Context1#dtl_context.block_trail]}, 
                            TreeWalkerAcc)
                end;
            ('inherit', TreeWalkerAcc) ->
                inherit_ast(Context, TreeWalkerAcc);
            ({'comment', _Contents}, TreeWalkerAcc) ->
                empty_ast(TreeWalkerAcc);
            ({'filter', Filters, Contents}, TreeWalkerAcc) ->
                filter_tag_ast(Filters, Contents, Context, TreeWalkerAcc);
			({'trans', {trans_text, _Pos, TransLiteral}}, TreeWalkerAcc) ->
				trans_ast(TransLiteral, Context, TreeWalkerAcc);
			({'trans_ext', {string_literal, _Pos, String}, Args}, TreeWalkerAcc) ->
				trans_ext_ast(String, Args, Context, TreeWalkerAcc);
            ({'date', 'now', {string_literal, _Pos, FormatString}}, TreeWalkerAcc) ->
                now_ast(FormatString, Context, TreeWalkerAcc);
            ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)}, 
                    TreeWalkerAcc);
            ({'text', _Pos, String}, TreeWalkerAcc) -> 
                string_ast(String, TreeWalkerAcc);
            ({'include', Template, Args, All}, TreeWalkerAcc) ->
                include_ast(Template, Args, All, Context, TreeWalkerAcc);
            ({'catinclude', Template, RscId, Args, All}, TreeWalkerAcc) ->
                catinclude_ast(Template, RscId, Args, All, Context, TreeWalkerAcc);
            ({'if', E, Contents, []}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifexpr_ast(E, IfAstInfo, [{'else', ElseAstInfo}], Context, TreeWalker2);
            ({'if', E, IfContents, ElseChoices}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                {ElseAsts,TW2} = lists:foldr(fun({'else', ElseContents}, {EAS, IfTW}) ->
                                                        {ElseAstInfo, IfTW1} = body_ast(ElseContents, Context, IfTW),
                                                        {[{'else', ElseAstInfo}|EAS], IfTW1};
                                                  ({'elseif', ElseIfExpr, ElseContents}, {EAS, IfTW}) ->
                                                        {ElseAstInfo, IfTW1} = body_ast(ElseContents, Context, IfTW),
                                                        {[{'elseif', ElseIfExpr, ElseAstInfo}|EAS], IfTW1}
                                             end,
                                             {[], TreeWalker1},
                                             ElseChoices),
                ifexpr_ast(E, IfAstInfo, ElseAsts, Context, TW2);
            ({'ifequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc), 
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                
            ({'ifnotequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(Contents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifnotequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(ElseContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(IfContents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                    
            ({'spaceless', Contents}, TreeWalkerAcc) ->
                spaceless_ast(Contents, Context, TreeWalkerAcc);
            ({'javascript', Contents}, TreeWalkerAcc) ->
                javascript_ast(Contents, Context, TreeWalkerAcc);
            ({'with', [ExprList, Identifiers], WithContents}, TreeWalkerAcc) ->
                with_ast(ExprList, Identifiers, WithContents, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Value}, Contents}, TreeWalkerAcc) ->
                for_loop_ast(IteratorList, Value, Contents, none, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Value}, Contents, EmptyPartContents}, TreeWalkerAcc) ->
                for_loop_ast(IteratorList, Value, Contents, EmptyPartContents, Context, TreeWalkerAcc);
            ({'load', Names}, TreeWalkerAcc) ->
                load_ast(Names, Context, TreeWalkerAcc);
            ({'tag', {'identifier', _, Name}, Args, All}, TreeWalkerAcc) ->
                tag_ast(Name, Args, All, Context, TreeWalkerAcc);
            ({'call_args', {'identifier', _, Name}, Args}, TreeWalkerAcc) ->
            	call_ast(Name, Args, Context, TreeWalkerAcc);
            ({'call_with', {'identifier', _, Name}, With}, TreeWalkerAcc) ->
            	call_with_ast(Name, With, Context, TreeWalkerAcc);
            ({'cycle', Names}, TreeWalkerAcc) ->
                cycle_ast(Names, Context, TreeWalkerAcc);
            ({'cycle_compat', Names}, TreeWalkerAcc) ->
                cycle_compat_ast(Names, Context, TreeWalkerAcc);
            ({'media', Variable, Args}, TreeWalkerAcc) ->
                media_ast(Variable, Args, Context, TreeWalkerAcc);
            ({'image', Variable, Args}, TreeWalkerAcc) ->
                image_ast(Variable, Args, Context, TreeWalkerAcc);
            ({'image_url', Variable, Args}, TreeWalkerAcc) ->
                image_url_ast(Variable, Args, Context, TreeWalkerAcc);
            ({'url', {'identifier', _, Name}, Args}, TreeWalkerAcc) ->
                url_ast(Name, Args, Context, TreeWalkerAcc);
            ({'print', Value}, TreeWalkerAcc) ->
                print_ast(Value, Context, TreeWalkerAcc);
            ({'lib', LibList, Args}, TreeWalkerAcc) ->
                lib_ast(LibList, Args, Context, TreeWalkerAcc);
            ({'cache', [MaxAge, Args], CacheContents}, TreeWalkerAcc) ->
                cache_ast(MaxAge, Args, CacheContents, Context, TreeWalkerAcc);
            ({'value', ValueToken, WithArgs}, TreeWalkerAcc) ->
                {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, WithArgs, true, Context, TreeWalkerAcc),
                {{format(ValueAst, Context),ValueInfo},ValueTreeWalker};
            (ValueToken, TreeWalkerAcc) -> 
                {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, Context, TreeWalkerAcc),
                {{format(ValueAst, Context),ValueInfo},ValueTreeWalker}
        end, TreeWalker, DjangoParseTree),
    
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
        fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) -> 
                PresetVars = lists:foldl(fun
                        (X, Acc) ->
                            case proplists:lookup(list_to_atom(X), Context#dtl_context.vars) of
                                none ->
                                    Acc;
                                Val ->
                                    [erl_syntax:abstract(Val) | Acc]
                            end
                    end, [], Info#ast_info.var_names),
                case PresetVars of
                    [] ->
                        {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                    _ ->
                        Counter = TreeWalkerAcc#treewalker.counter,
                        Name = lists:concat([pre_render, Counter]),
                        Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                            [erl_syntax:list(PresetVars)]),
                        PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                            [erl_syntax:clause([erl_syntax:variable("Variables")], none, [Ast])]),
                        PreRenderAsts = Info#ast_info.pre_render_asts,
                        Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},     
                        {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                end
        end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


merge_info(Info1, Info2) ->
    #ast_info{dependencies = 
        lists:merge(
            lists:sort(Info1#ast_info.dependencies), 
            lists:sort(Info2#ast_info.dependencies)),
        var_names = 
            lists:merge(
                lists:sort(Info1#ast_info.var_names), 
                lists:sort(Info2#ast_info.var_names)),
        pre_render_asts = 
            lists:merge(
                Info1#ast_info.pre_render_asts,
                Info2#ast_info.pre_render_asts)}.


%with_dependencies([], Args) ->
%    Args;
%with_dependencies([H, T], Args) ->
%     with_dependencies(T, with_dependency(H, Args)).
%        
with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [{FilePath, filelib:last_modified(FilePath)} | Info#ast_info.dependencies]}}, TreeWalker}.



inherit_ast(Context, TreeWalker) ->
    {BlockName,BlockFile} = hd(Context#dtl_context.block_trail),
    Inherited = [ {F, dict:find({BlockName,F}, Context#dtl_context.block_dict)} 
                || F <- find_prev_all(Context#dtl_context.parse_trail, BlockFile, []) ],
    case [ {F,C} || {F,{ok, C}} <- Inherited ] of
        [{InheritedFile,Content}|_] ->
            body_ast(Content,
                     Context#dtl_context{block_trail=[{BlockName,InheritedFile}|Context#dtl_context.block_trail]}, 
                     TreeWalker);
        [] ->
            {{erl_syntax:string(""), #ast_info{}}, TreeWalker}
    end.

    find_prev_all([], _Find, Acc) -> Acc;
    find_prev_all([Find|_], Find, Acc) -> Acc;
    find_prev_all([F|Rest], Find, Acc) -> find_prev_all(Rest, Find, [F|Acc]).


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.

value_ast(ValueToken, Args, AsString, Context, TreeWalker) ->
    value_ast(ValueToken, Args, AsString, Context, TreeWalker, []).

value_ast(ValueToken, [], AsString, Context, TreeWalker, []) ->
    value_ast(ValueToken, AsString, Context, TreeWalker);
value_ast(ValueToken, [], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_context),
                                           erl_syntax:atom(set),
                                           [erl_syntax:atom(extra_args),
                                            erl_syntax:list([ erl_syntax:tuple([erl_syntax:atom(X),XAst]) || {X,XAst} <- ExtraArgs]),
                                            z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'ZpContext', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = value_ast(ValueToken, AsString, WithContext, TreeWalker),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,"sudo"}, true}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_acl),
                                           erl_syntax:atom(sudo),
                                           [z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'ZpContext', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,"anondo"}, true}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_acl),
                                           erl_syntax:atom(anondo),
                                           [z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'ZpContext', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,"z_language"}, Lang}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    {{LangAst,InfoValue1}, TreeWalker1} = value_ast(Lang, false, Context, TreeWalker),
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_context), 
                                           erl_syntax:atom(set_language),
                                           [LangAst, z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'ZpContext', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue2}, TreeWalker2} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker1, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
value_ast(ValueToken, [{{identifier,_,Var}, Value}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    {{ValueAst,InfoValue1}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
    VarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    WithContext = Context#dtl_context{local_scopes=[ [{list_to_atom(Var), VarAst}] | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue2}, TreeWalker2} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker1, [{list_to_atom(Var), VarAst}|ExtraArgs]),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(VarAst, ValueAst), InnerAst]),
    {{WithAst, merge_info(InfoValue1,InfoValue2)}, TreeWalker2}.
    

value_ast(ValueToken, AsString, Context, TreeWalker) ->
    case ValueToken of
        {'expr', Operator, Value} ->
            {{ValueAst,InfoValue}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_operators), 
                                         erl_syntax:atom(Operator), 
                                         [ValueAst, z_context_ast(Context)]),
            {{Ast, InfoValue}, TreeWalker1};
        {'expr', Operator, Value1, Value2} ->
            {{Value1Ast,InfoValue1}, TreeWalker1} = value_ast(Value1, false, Context, TreeWalker),
            {{Value2Ast,InfoValue2}, TreeWalker2} = value_ast(Value2, false, Context, TreeWalker1),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_operators), 
                                         erl_syntax:atom(Operator), 
                                         [Value1Ast, Value2Ast, z_context_ast(Context)]),
            {{Ast, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
        {'string_literal', _Pos, String} ->
            {{auto_escape(erl_syntax:string(unescape_string_literal(String)), Context), 
                    #ast_info{}}, TreeWalker};
		{'trans_literal', _Pos, String} ->
            {{auto_escape(trans_literal_ast(String, Context), Context), 
                    #ast_info{}}, TreeWalker};
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, TreeWalker);
                false -> {{erl_syntax:integer(list_to_integer(Number)), #ast_info{}}, TreeWalker}
            end;
        {'atom_literal', _Pos, String} ->
            {{erl_syntax:atom(to_atom(unescape_string_literal(String))), #ast_info{}}, TreeWalker};
        undefined ->
            {{erl_syntax:atom(undefined), #ast_info{}}, TreeWalker};
        {'auto_id', Name} ->
            auto_id_ast(Name, Context, TreeWalker);
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo,#ast_info{var_names = [VarName]})}, TreeWalker1};
        {'variable', _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo, #ast_info{var_names = [VarName]})}, TreeWalker1};
        {'index_value', _, _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_indexvariable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo, #ast_info{var_names = [VarName]})}, TreeWalker1};
        {tuple_value, {identifier, _, TupleName}, TupleArgs} ->
            TupleNameAst = erl_syntax:atom(TupleName),
            {TupleArgsAst, TreeWalker1} = scomp_ast_list_args(TupleArgs, Context, TreeWalker),
            {{erl_syntax:tuple([TupleNameAst, TupleArgsAst]), #ast_info{}}, TreeWalker1};
        {value_list, Values} ->
            {ValueAstList, ValueInfo, TreeWalker1} = lists:foldl(
                        fun(V, {Acc,Info,TreeW}) ->
                            {{Ast,InfoV}, TreeW1} = value_ast(V, false, Context, TreeW),
                            {[Ast|Acc], merge_info(Info,InfoV), TreeW1}
                        end,
                        {[], #ast_info{}, TreeWalker}, 
                        Values),
            {{erl_syntax:list(lists:reverse(ValueAstList)), ValueInfo},TreeWalker1}
    end.

string_ast(String, TreeWalker) ->
    % {{erl_syntax:string(String), #ast_info{}}, TreeWalker}. %% less verbose AST, better for development and debugging
    {{erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]), #ast_info{}}, TreeWalker}.       

catinclude_ast(File, Id, Args, All, Context, TreeWalker) ->
    Args1 = [ {{identifier, none, "$file"}, File},
			  {{identifier, none, "id"}, Id} | Args],
    scomp_ast("catinclude", Args1, All, Context, TreeWalker).


include_ast(File, Args, All, Context, TreeWalker) ->
    {UseScomp, IsSudo} = lists:foldl( fun({{identifier, _, Key}, Val}, {IsC,IsSu}) -> 
                                case Key of
                                    "maxage" -> {true, IsSu};
                                    "vary"   -> {true, IsSu};
                                    "scomp"  -> {true, IsSu};
                                    "visible_for" -> {true, IsSu};
                                    "sudo" ->
                                        case Val of
                                            true -> {IsC, true};
                                            _ -> {IsC, IsSu}
                                        end;
                                    _ -> {IsC, IsSu}
                                end
                            end,
                            {false, false},
                            Args),
    case {UseScomp, File} of
        {false, {string_literal, _, Template}} ->
            Template1 = unescape_string_literal(Template),
            {InterpretedArgs, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
            {ScopedArgs, ArgAsts} = lists:foldr(
                fun({AKey, AAst}, {ScopeAcc, AstAcc}) ->
                    Var = "Arg_" ++ z_ids:identifier(10),
                    AssignAst = erl_syntax:match_expr(erl_syntax:variable(Var), AAst),
                    { [{AKey, erl_syntax:variable(Var)}|ScopeAcc], [AssignAst|AstAcc] }
                end,
                {[], []},
                InterpretedArgs),

            {ContextInclude,ArgAsts1} = case IsSudo of
                                true -> 
                                    V = "ZpContext_" ++ z_ids:id(10), 
                                    ZpContextAst = erl_syntax:match_expr(
                                                        erl_syntax:variable(V),
                                                        erl_syntax:application(
                                                                    erl_syntax:atom(z_acl),
                                                                    erl_syntax:atom(sudo),
                                                                    [z_context_ast(Context)])),
                                    LocalScope = [{'ZpContext', erl_syntax:variable(V)}],
                                    { Context#dtl_context{local_scopes=[LocalScope|Context#dtl_context.local_scopes]},
                                      [ZpContextAst|ArgAsts] 
                                    };
                                false -> 
                                    {Context, ArgAsts}
                             end,

            % {AstList, Info, TreeWalker}
            IncludeFun = fun(FilePath, {AstList, InclInfo, TreeW}) ->
                    notify(#debug{what=template, arg={include, File, FilePath}}, ContextInclude#dtl_context.z_context),
                    case parse(FilePath, ContextInclude) of
                        {ok, InclusionParseTree} ->
                            AutoIdVar = "AutoId_"++z_ids:identifier(),
                            IncludeScope = [ {'$autoid', erl_syntax:variable(AutoIdVar)} | ScopedArgs ],

                            {{Ast,Info}, InclTW2} = 
                                            with_dependency(FilePath, 
                                                    body_ast(
                                                        InclusionParseTree,
                                                        Context#dtl_context{
                                                                local_scopes = [ IncludeScope | ContextInclude#dtl_context.local_scopes ],
                                                                parse_trail = [FilePath | ContextInclude#dtl_context.parse_trail]}, 
                                                        TreeW#treewalker{has_auto_id=false})),
                            Ast1 = case InclTW2#treewalker.has_auto_id of
                                false -> Ast;
                                true ->  erl_syntax:block_expr(
                                            [
                                            erl_syntax:match_expr(
                                                    erl_syntax:variable(AutoIdVar), 
                                                    erl_syntax:application(
                                                        erl_syntax:atom(z_ids),
                                                        erl_syntax:atom(identifier),
                                                        [])),
                                            Ast])
                            end,
                            {[Ast1|AstList], merge_info(InclInfo, Info), InclTW2#treewalker{has_auto_id=TreeW#treewalker.has_auto_id}};
                        Err ->
                            throw(Err)
                    end
            end,

            % Compile all included files, put them in a block expr with a single assignment of the argument vars at the start.
            case lists:foldl(IncludeFun, {[], #ast_info{}, TreeWalker1}, full_path(Template1, All, Context)) of
                {[], _, TreeWalkerN} ->
                    case All of
                        false -> ?LOG("include_ast: could not find template ~p", [Template1]);
                        true -> ok
                    end,
                    {{erl_syntax:string(""), #ast_info{}}, TreeWalkerN};
                {AstList, AstInfo, TreeWalkerN} ->
                    AstN = erl_syntax:block_expr(ArgAsts1 ++ [erl_syntax:list(lists:reverse(AstList))]),
                    {{AstN, AstInfo}, TreeWalkerN}
            end;
        _ ->
            Args1 = [{{identifier, none, "$file"},File} | Args],
            scomp_ast("include", Args1, All, Context, TreeWalker)
    end.

filter_tag_ast(FilterList, Contents, Context, TreeWalker) ->
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents, Context#dtl_context{auto_escape = did}, TreeWalker),

    {{FilteredAst, FilteredInfo}, TreeWalker2} = lists:foldl(fun
                ({filter, {identifier, _, "escape"}, _}, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                    {{AstAcc, InfoAcc}, TreeWalkerAcc};
                (Filter, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                    {{Ast, AstInfo}, TreeWalkerAcc1} = filter_ast1(Filter, AstAcc, Context, TreeWalkerAcc),
                    {{Ast, merge_info(InfoAcc, AstInfo)}, TreeWalkerAcc1}
            end, {{erl_syntax:application(
                        erl_syntax:atom(lists),
                        erl_syntax:atom(flatten),
                        [InnerAst]), Info}, TreeWalker1}, FilterList),

    case search_for_escape_filter(lists:reverse(FilterList), Context) of
        on -> 
            {{erl_syntax:application(
                erl_syntax:atom(filter_force_escape),
                erl_syntax:atom(force_escape),
                [FilteredAst, z_context_ast(Context)]), FilteredInfo}, TreeWalker2};
        _ -> 
            {{FilteredAst, FilteredInfo}, TreeWalker2}
    end.

filter_ast(Variable, Filter, Context, TreeWalker) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(Variable, Filter, Context#dtl_context{auto_escape = did}, TreeWalker),
    case search_for_escape_filter(Variable, Filter, Context) of
        on -> 
            {{erl_syntax:application(
                        erl_syntax:atom(filter_force_escape), 
                        erl_syntax:atom(force_escape), 
                        [UnescapedAst, z_context_ast(Context)]), Info}, TreeWalker};
        _ -> 
            {{UnescapedAst, Info}, TreeWalker2}
    end.

filter_ast_noescape(Variable, {filter, {identifier, _, "escape"}, []}, Context, TreeWalker) ->
    value_ast(Variable, true, Context, TreeWalker);
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{VariableAst,Info},TreeWalker2} = value_ast(Variable, true, Context, TreeWalker),
    {{FilterAst,Info2},TreeWalker3} = filter_ast1(Filter, VariableAst, Context, TreeWalker2),
    {{FilterAst, merge_info(Info, Info2)}, TreeWalker3}.

filter_ast1({filter, {identifier, _, Name}, []}, VariableAst, Context, TreeWalker) ->
    FilterAst = erl_syntax:application(erl_syntax:atom(list_to_atom("filter_"++Name)), erl_syntax:atom(Name), [VariableAst, z_context_ast(Context)]),
    {{FilterAst, #ast_info{}}, TreeWalker};
filter_ast1({filter, {identifier, _, "default"}, [Arg]}, VariableAst, Context, TreeWalker) ->
    {{ArgAst, Info},TreeWalker1} = value_ast(Arg, false, Context, TreeWalker),
    VarAst  = erl_syntax:variable("Default_" ++ z_ids:identifier()),
    CaseAst = erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_false), [VarAst, z_context_ast(Context)]),
        [erl_syntax:clause([erl_syntax:atom(true)], none, 
                [ArgAst]),
         erl_syntax:clause([erl_syntax:underscore()], none,
                [VarAst])
        ]),
    {{erl_syntax:block_expr([erl_syntax:match_expr(VarAst, VariableAst), CaseAst]), Info}, TreeWalker1};
filter_ast1({filter, {identifier, _, "default_if_none"}, [Arg]}, VariableAst, Context, TreeWalker) ->
    {{ArgAst, Info},TreeWalker1} = value_ast(Arg, false, Context, TreeWalker),
    VarAst  = erl_syntax:variable("Default_" ++ z_ids:identifier()),
    CaseAst = erl_syntax:case_expr(VariableAst,
        [erl_syntax:clause([erl_syntax:atom(undefined)], none, 
                [ArgAst]),
         erl_syntax:clause([VarAst], none,
                [VarAst])
        ]),
    {{CaseAst, Info}, TreeWalker1};
filter_ast1({filter, {identifier, Pos, "default_if_undefined"}, Args}, VariableAst, Context, TreeWalker) ->
    filter_ast1({filter, {identifier, Pos, "default_if_none"}, Args}, VariableAst, Context, TreeWalker);
filter_ast1({filter, {identifier, _, Name}, Args}, VariableAst, Context, TreeWalker) ->
    {{ArgAsts, Info}, TreeWalker2} = lists:foldr(
                        fun(Arg, {{As,In},Tw}) ->
                            {{ArgAst,ArgIn}, Tw1} = value_ast(Arg, false, Context, Tw),
                            {{[ArgAst|As], merge_info(In,ArgIn)}, Tw1}
                        end,
                        {{[], #ast_info{}}, TreeWalker},
                        Args),
    FilterAst = erl_syntax:application(
                    erl_syntax:atom(list_to_atom("filter_"++Name)), 
                    erl_syntax:atom(Name), 
                    [VariableAst|ArgAsts] ++ [z_context_ast(Context)]
                ),
    {{FilterAst, Info}, TreeWalker2}.
    

search_for_escape_filter(_FilterList, #dtl_context{auto_escape = on}) ->
    on;
search_for_escape_filter(_FilterList, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter([{filter, {identifier, _, "escape"}, []}|_Rest], _Context) ->
    on;
search_for_escape_filter([_|Rest], Context) ->
    search_for_escape_filter(Rest, Context);
search_for_escape_filter([], _Context) ->
    off.

 
search_for_escape_filter(_, _, #dtl_context{auto_escape = on}) ->
    on;
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(_, {filter, {identifier, _, "escape"}, []}, _Context) ->
    on;
search_for_escape_filter({apply_filter, Variable, Filter}, _, Context) ->
    search_for_escape_filter(Variable, Filter, Context);
search_for_escape_filter(_Variable, _Filter, _Context) ->
    off.

resolve_variable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'fetch_value'), Context).

resolve_ifvariable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'find_value'), Context).

resolve_indexvariable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'fetch_value'), Context).


opttrans_variable_ast({{Ast, VarName, Info}, TreeWalker}, Context) ->
    Ast1 = erl_syntax:application(
            erl_syntax:atom(z_trans), 
            erl_syntax:atom(lookup_fallback),
			[
				Ast,
				z_context_ast(Context)
			]),
	{{Ast1, VarName, Info}, TreeWalker}.

resolve_variable_ast({index_value, Variable, Index}, Context, TreeWalker, FinderFunction) ->
    {{IndexAst,Info},TreeWalker2} = value_ast(Index, false, Context, TreeWalker),
    {{VarAst, VarName, Info2}, TreeWalker3} = resolve_variable_ast(Variable, Context, TreeWalker2, FinderFunction),
    Ast = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime), 
            erl_syntax:atom(FinderFunction),
            [IndexAst, VarAst, z_context_ast(Context)]),
    {{Ast, VarName, merge_info(Info, Info2)}, TreeWalker3};

resolve_variable_ast({attribute, {{identifier, _, Arg}, {variable, {identifier, _, "q"}}}}, Context, TreeWalker, _FinderFunction) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(z_context), 
            erl_syntax:atom(get_q),
            [erl_syntax:string(Arg), z_context_ast(Context)]),
    {{Ast, "q", #ast_info{}}, TreeWalker};

resolve_variable_ast({attribute, {{identifier, _, Arg}, {variable, {identifier, _, "q_validated"}}}}, Context, TreeWalker, _FinderFunction) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(z_context), 
            erl_syntax:atom(get_q_validated),
            [erl_syntax:string(Arg), z_context_ast(Context)]),
    {{Ast, "q", #ast_info{}}, TreeWalker};

resolve_variable_ast({attribute, {{identifier, _, Model}, {variable, {identifier, _, "m"}}}}, _Context, TreeWalker, _FinderFunction) ->
    Ast = erl_syntax:tuple([
            erl_syntax:atom(m),
            erl_syntax:atom("m_" ++ Model),
            erl_syntax:atom(undefined)
        ]),
    {{Ast, "m", #ast_info{}}, TreeWalker};

resolve_variable_ast({attribute, {{identifier, _, AttrName}, Variable}}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarName, Info}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    Ast = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime),
            erl_syntax:atom(FinderFunction),
            [erl_syntax:atom(AttrName), VarAst, z_context_ast(Context)]),
    {{Ast, VarName, Info}, TreeWalker2};

resolve_variable_ast({variable, {identifier, _, "now"}}, Context, TreeWalker, _FinderFunction) ->
    Ast = case resolve_scoped_variable_ast("now", Context) of
        undefined ->
            erl_syntax:application(
                erl_syntax:atom(erlang),
                erl_syntax:atom(localtime),
                []);
        Val ->
            Val
    end,
    {{Ast, "now", #ast_info{}}, TreeWalker};

resolve_variable_ast({variable, {identifier, _, "z_language"}}, Context, TreeWalker, _FinderFunction) ->
    Ast = case resolve_scoped_variable_ast("z_language", Context) of
        undefined ->
            erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(language),
                [z_context_ast(Context)]);
        Val ->
            Val
    end,
    {{Ast, "z_language", #ast_info{}}, TreeWalker};


resolve_variable_ast({variable, {identifier, _, VarName}}, Context, TreeWalker, FinderFunction) ->
    Ast = case resolve_scoped_variable_ast(VarName, Context) of
        undefined ->
            erl_syntax:application(
                erl_syntax:atom(erlydtl_runtime), 
                erl_syntax:atom(FinderFunction),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables"), z_context_ast(Context)]);
        Val ->
            Val
    end,
    {{Ast, VarName, #ast_info{}}, TreeWalker};

resolve_variable_ast({apply_filter, Variable, Filter}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarName, Info}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    ValueAst = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime),
            erl_syntax:atom(to_value),
            [VarAst, z_context_ast(Context)]
        ),
    {{VarValue, Info2}, TreeWalker3} = filter_ast1(Filter, ValueAst, Context, TreeWalker2),
    {{VarValue, VarName, merge_info(Info, Info2)}, TreeWalker3};

resolve_variable_ast(ValueToken, Context, TreeWalker, _FinderFunction) ->
    {{Ast, Info}, TreeWalker1} = value_ast(ValueToken, false, Context, TreeWalker),
    {{Ast, "$value", Info}, TreeWalker1}.


resolve_scoped_variable_ast(VarName, Context) when is_atom(VarName) ->
    lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined -> proplists:get_value(VarName, Scope);
                    _ -> Value
                end
        end, undefined, Context#dtl_context.local_scopes);
resolve_scoped_variable_ast(VarName, Context) when is_list(VarName) ->
    resolve_scoped_variable_ast(list_to_atom(VarName), Context).



%% @doc Return the AST for the z_context var
z_context_ast(Context) ->
    case resolve_scoped_variable_ast('ZpContext', Context) of
        undefined -> erl_syntax:variable("ZpContext"); 
        Ast -> Ast
    end.


format(Ast, Context) ->
    auto_escape(stringify(Ast, Context), Context).

stringify(Ast, Context) ->
    erl_syntax:application(erl_syntax:atom(filter_stringify), erl_syntax:atom(stringify),
        [Ast, z_context_ast(Context)]).

auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(filter_force_escape), erl_syntax:atom(force_escape),
                [Value, z_context_ast(Context)]);
        _ ->
            Value
    end.

ifexpr_ast(Expression, IfContents, ElseChoices, Context, TreeWalker) ->
    lists:foldr(fun({'else', {ElseAst, ElseInfo}}, {{_InnerAst, Inf}, TW}) ->
                            {{ElseAst, merge_info(Inf, ElseInfo)}, TW};
                   ({'elseif', E, {ElseAst, ElseInfo}}, {{InnerAst, Inf}, TW}) ->
                             {{ElseExprAst, ElseExprInfo}, TW1} = value_ast(E, false, Context, TW),
                             {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
                                                                          erl_syntax:atom(is_false), 
                                                                          [ElseExprAst, z_context_ast(Context)]),
                                                    [
                                                    erl_syntax:clause([erl_syntax:atom(true)], none, [InnerAst]),
                                                    erl_syntax:clause([erl_syntax:underscore()], none, [ElseAst])
                                                    ]),
                               merge_info(merge_info(Inf, ElseInfo), ElseExprInfo)
                              }, TW1}
                end,
                empty_ast(TreeWalker),
                [{'elseif', Expression, IfContents} | ElseChoices]).


ifequalelse_ast(Args, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {[Arg1Ast, Arg2Ast], VarNames, Info1, TreeWalker1} = lists:foldl(fun
            (X, {Asts, AccVarNames, Inf, TW}) ->
                case X of
					{string_literal, _, Literal} ->
					    {[erl_syntax:string(unescape_string_literal(Literal)) | Asts], AccVarNames, Inf, TW};
				    {trans_literal, _, Literal} ->
				        {[trans_literal_ast(Literal, Context) | Asts], AccVarNames, Inf, TW};
                    {number_literal, _, Literal} ->
                        {[erl_syntax:integer(list_to_integer(Literal)) | Asts], AccVarNames, Inf, TW};
                    Variable ->
                        {{Ast, VarName, VarInfo}, TW1} = resolve_ifvariable_ast(Variable, Context, TW),
                        {[Ast | Asts], [VarName | AccVarNames], merge_info(Inf, VarInfo), TW1}
                end                
        end,
        {[], Info#ast_info.var_names, #ast_info{}, TreeWalker},
        Args),
    Ast = erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(are_equal),
            [Arg1Ast, Arg2Ast]),
        [
            erl_syntax:clause([erl_syntax:atom(true)], none, [IfContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none, [ElseContentsAst])
        ]),
    {{Ast, merge_info(Info1, Info#ast_info{var_names = VarNames})}, TreeWalker1}.         


%% With statement with only a single variable, easy & quick match.
with_ast([Value], [{identifier, _, V}], Contents, Context, TreeWalker) ->
    Postfix = z_ids:identifier(),
    VarAst  = erl_syntax:variable("With_" ++ V ++ [$_|Postfix]),
    {{ValueAst, ValueInfo}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
    LocalScope = [ {list_to_atom(V), VarAst} ],
    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(
            Contents,
            Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]}, 
            TreeWalker1),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(VarAst, ValueAst), InnerAst]),
    {{WithAst, merge_info(ValueInfo,InnerInfo)}, TreeWalker2};
    
%% With statement with multiple vars, match against tuples and lists.
with_ast([Value], Variables, Contents, Context, TreeWalker) ->
    Postfix = z_ids:identifier(),
    VarAsts = lists:map(fun({identifier, _, V}) -> 
                    erl_syntax:variable("With_" ++ V ++ [$_|Postfix]) 
            end, Variables),
    {{ValueAst, ValueInfo}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
    LocalScope = lists:map( fun({identifier, _, V}) ->
                                    {list_to_atom(V), erl_syntax:variable("With_" ++ V ++ [$_|Postfix]) } 
                            end, Variables),
    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(
            Contents,
            Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]}, 
            TreeWalker1),
            
    ListClauseAst = erl_syntax:clause([erl_syntax:list(VarAsts)], none, [InnerAst]),
    TupleClauseAst = erl_syntax:clause([erl_syntax:tuple(VarAsts)], none, [InnerAst]),
    WithAst = erl_syntax:case_expr(ValueAst, [ListClauseAst, TupleClauseAst]),
    {{WithAst, merge_info(ValueInfo,InnerInfo)}, TreeWalker2};

%% With statement with multiple expressions and multiple vars
with_ast(ValueList, Variables, Contents, Context, TreeWalker) ->
    Postfix = z_ids:identifier(),
    VarAsts = lists:map(fun({identifier, _, V}) -> 
                            erl_syntax:variable("With_" ++ V ++ [$_|Postfix]) 
                        end, Variables),
    {{ValueAsts, ValueInfo}, TreeWalker1} = lists:foldr(
                        fun (V,{{Vs,Inf},TW}) ->
                            {{VAst, VInfo}, TW1} = value_ast(V, false, Context, TW),
                            {{[VAst|Vs], merge_info(VInfo,Inf)}, TW1}
                        end,
                        {{[],#ast_info{}}, TreeWalker},
                        ValueList),
    LocalScope = lists:map( fun({identifier, _, V}) ->
                                {list_to_atom(V), erl_syntax:variable("With_" ++ V ++ [$_|Postfix]) } 
                            end, Variables),
    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(
            Contents,
            Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]}, 
            TreeWalker1),
    Assignments = [ erl_syntax:match_expr(Var,Val) || {Var,Val} <- lists:zip(VarAsts,ValueAsts) ],
    WithAst = erl_syntax:block_expr(Assignments ++ [InnerAst]),
    {{WithAst, merge_info(ValueInfo,InnerInfo)}, TreeWalker2}.



for_loop_ast(IteratorList, LoopValue, Contents, EmptyPartContents, Context, TreeWalker) ->
    PostFix = z_ids:identifier(),
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator ++ PostFix) 
            end, IteratorList),
    {{InnerAst, Info}, TreeWalker2} = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{'forloop', erl_syntax:variable("Counters")} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator ++ PostFix)} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}, TreeWalker),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
        erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),

    {{LoopValueAst, LoopValueInfo}, TreeWalker3} = value_ast(LoopValue, false, Context, TreeWalker2),
    ListAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(to_list), [LoopValueAst, z_context_ast(Context)]),
    ListVarAst = erl_syntax:variable("LoopVar_"++z_ids:identifier()),

    CounterVars0 = case resolve_scoped_variable_ast("forloop", Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListVarAst]);
        ForLoopValue ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListVarAst, ForLoopValue])
    end,

    ForLoopF = fun(BaseListAst) -> erl_syntax:application(
            erl_syntax:atom('erlang'), erl_syntax:atom('element'),
            [erl_syntax:integer(1), erl_syntax:application(
                    erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                    [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
                                        _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])])
                            ]),
                        CounterVars0, BaseListAst])])
    end,

    {CompleteForLoopAst, Info2, TreeWalker4} = case EmptyPartContents of
        none ->
            {ForLoopF(ListVarAst), Info, TreeWalker3};
        _ -> 
            {{EmptyPartAst, EmptyPartInfo}, EmptyWalker} = body_ast(EmptyPartContents, Context, TreeWalker3),
            LAst = erl_syntax:variable("L_"++z_ids:identifier()),
            EmptyClauseAst = erl_syntax:clause(
        		 [erl_syntax:list([])], 
        		 none,
        		 [EmptyPartAst]),
            LoopClauseAst = erl_syntax:clause(
                [LAst],
                none,
                [ForLoopF(LAst)]),
            {erl_syntax:case_expr(ListVarAst, [EmptyClauseAst, LoopClauseAst]), merge_info(Info,EmptyPartInfo), EmptyWalker} 
    end,
    ForBlockAst = erl_syntax:block_expr([
                        erl_syntax:match_expr(ListVarAst, ListAst),
                        CompleteForLoopAst]),
    {{ForBlockAst, merge_info(LoopValueInfo, Info2)}, TreeWalker4}.

load_ast(Names, _Context, TreeWalker) ->
    CustomTags = lists:merge([X || {identifier, _ , X} <- Names], TreeWalker#treewalker.custom_tags),
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker#treewalker{custom_tags = CustomTags}}.  

cycle_ast(Names, Context, TreeWalker) ->
    {NamesTuple, TreeWalker1} = lists:foldr(
                        fun
                        ({string_literal, _, Str}, {Acc,TW}) ->
                            {[ erl_syntax:string(unescape_string_literal(Str)) | Acc], TW};
						({trans_literal, _, Str}, {Acc,TW}) ->
						  	{[ trans_literal_ast(Str, Context) | Acc ], TW};
                        ({atom_literal, _, Str}, {Acc,TW}) ->
                            {[ erl_syntax:atom(to_atom(unescape_string_literal(Str))) | Acc], TW};
                        ({number_literal, _, Num}, {Acc,TW}) ->
                            V = format(erl_syntax:integer(Num), Context),
                            {[ V | Acc ], TW};
                        ({variable, _}=Var, {Acc,TW}) ->
                            {{V, _VarName, _VarInfo},TW2}  = resolve_variable_ast(Var, Context, TW),
                            {[ V | Acc ], TW2};
                        ({auto_id, Name}, {Acc,TW}) ->
                            {{V, _}, TW1} = auto_id_ast(Name, Context, TW),
                            {[ V |Acc ], TW1};
                        (_, {Acc,TW}) ->
                           {[ erl_syntax:atom(undefined) | Acc ], TW}
                        end,
                        {[],TreeWalker},
                        Names),

    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters"), z_context_ast(Context)]), #ast_info{}}, TreeWalker1}.


%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, Context, TreeWalker) ->
    NamesTuple = [erl_syntax:string(X) || {identifier, _, X} <- Names],
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters"), z_context_ast(Context)]), #ast_info{}}, TreeWalker}.


%% @doc Output the trans record with the translation call to z_trans.
%% author: Marc Worrell
trans_ast(TransLiteral, Context, TreeWalker) ->
	% Remove the first and the last character, these were separating the string from the {_ and _} tokens
	{{trans_ast1(z_string:trim(TransLiteral), Context), #ast_info{}}, TreeWalker}.

trans_ext_ast(String, Args, Context, TreeWalker) ->
	Lit = unescape_string_literal(String, [], noslash),
	ArgsTrans = [ trans_arg(A) || A <- Args ],
	{{trans_ast1({trans, [{en,Lit}|ArgsTrans]}, Context), #ast_info{}}, TreeWalker}.

    trans_arg({{identifier,_,Lang}, {string_literal,_,String}}) ->
        {list_to_atom(Lang), String}.
        
trans_literal_ast(String, Context) ->
	Lit = unescape_string_literal(String),
	trans_ast1(Lit, Context).


%% @doc Fetch the translations and put them into the compiled template.  We will need to
%% re-compile templates when translations are changed.
trans_ast1(Arg, Context) ->
    case z_trans:translations(Arg, Context#dtl_context.z_context) of
        {trans, Tr} ->
        	Tr1 = [ {z_convert:to_atom(Lang), z_convert:to_binary(S)} || {Lang,S} <- Tr ],
        	erl_syntax:application(
        		erl_syntax:atom(z_trans),
        		erl_syntax:atom(trans),
        		[
        			erl_syntax:abstract({trans, Tr1}),
        			z_context_ast(Context)
        		]);
        S when is_binary(S) ->
            erl_syntax:abstract(S);
        L when is_list(L) ->
            erl_syntax:abstract(list_to_binary(L))
    end.
	


now_ast(FormatString, Context, TreeWalker) ->
    % Note: we can't use unescape_string_literal here
    % because we want to allow escaping in the format string.
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [erl_syntax:string(FormatString), z_context_ast(Context)]),
        #ast_info{}}, TreeWalker}.

spaceless_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{erl_syntax:application(erl_syntax:atom(erlydtl_runtime),
                             erl_syntax:atom(spaceless),
                             [Ast]), Info}, TreeWalker1}.

javascript_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{erl_syntax:application(erl_syntax:atom(z_script),
                             erl_syntax:atom(javascript_ast),
                             [Ast, z_context_ast(Context)]), Info}, TreeWalker1}.

unescape_string_literal(String) ->
    unescape_string_literal(String, [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).


to_atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B));
to_atom(L) when is_list(L) ->
    list_to_atom(L);
to_atom(A) when is_atom(A) ->
    A.


full_path(File, Context) ->
    case full_path(File, false, Context) of
        [Filename] -> {ok, Filename};
        [] -> {error, enoent}
    end.

full_path(File, All, Context) ->
    case Context#dtl_context.finder of
        undefined ->
            case Context#dtl_context.z_context of
                undefined -> [];
                ZContext -> z_template:find_template(File, All, ZContext)
            end;
        FinderFun ->
            FinderFun(File, All)
    end.


%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

tag_ast(Name, Args, All, Context, TreeWalker) ->
    case lists:member(Name, TreeWalker#treewalker.custom_tags) of
        true ->
            {InterpretedArgs, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
            DefaultFilePath = filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags", Name]),
            case Context#dtl_context.custom_tags_dir of
                [] ->
                    case parse(DefaultFilePath, Context) of
                        {ok, TagParseTree} ->
                            tag_ast2(DefaultFilePath, TagParseTree, InterpretedArgs, Context, TreeWalker1);
                        _ ->
                            Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                DefaultFilePath]),
                            throw({error, Reason})
                    end;
                _ ->
                    CustomFilePath = filename:join([Context#dtl_context.custom_tags_dir, Name]),
                    case parse(CustomFilePath, Context) of
                        {ok, TagParseTree} ->
                            tag_ast2(CustomFilePath,TagParseTree, InterpretedArgs, Context, TreeWalker1);
                        _ ->
                            case parse(DefaultFilePath, Context) of
                                {ok, TagParseTree} ->
                                    tag_ast2(DefaultFilePath, TagParseTree, InterpretedArgs, Context, TreeWalker1);
                                _ ->
                                    Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                        CustomFilePath, ", ", DefaultFilePath]),
                                    throw({error, Reason})
                            end
                    end
            end;
        _ ->
            % Dynamic scomp call
            % throw({error, lists:concat(["Custom tag '", Name, "' not loaded"])})
            scomp_ast(Name, Args, All, Context, TreeWalker)
    end.
 
 tag_ast2(Source, TagParseTree, InterpretedArgs, Context, TreeWalker) ->
    with_dependency(Source, body_ast(TagParseTree, Context#dtl_context{
        local_scopes = [ InterpretedArgs | Context#dtl_context.local_scopes ],
        parse_trail = [ Source | Context#dtl_context.parse_trail ]}, TreeWalker)).


call_ast(Module, Args, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    call_ast(Module, ArgsAst, #ast_info{}, Context, TreeWalker1).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {{VarAst, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
    call_ast(Module, VarAst, merge_info(VarInfo, #ast_info{var_names=[VarName]}), Context, TreeWalker1).
        
call_ast(Module, ArgAst, AstInfo, Context, TreeWalker) ->
     AppAst = erl_syntax:application(
		erl_syntax:atom(Module),
		erl_syntax:atom(render),
		[   ArgAst,
		    erl_syntax:variable("Variables"),
		    z_context_ast(Context)
		]),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
	      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
	      none,
	      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
		  erl_syntax:atom(io_lib),
		  erl_syntax:atom(format),
		  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
		 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
		 none,
		 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),   
    {{CallAst, AstInfo}, TreeWalker}.


%% @doc Generate html to show the media tag.  This is different in that the image tag can only
%% display images using the <img /> tag.  This can also generate complete media viewers.
%% Author: Marc Worrell
%% @todo Optimization for the situation where all parameters are constants
media_ast(FilenameValue, Args, Context, TreeWalker) ->
    FilenameAst = resolve_value_ast(FilenameValue, Context, TreeWalker),
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                        erl_syntax:atom(z_media_tag),
                        erl_syntax:atom(viewer),
                        [   FilenameAst,
                            ArgsAst,
                            z_context_ast(Context)
                        ]
                    ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
                      none,
                      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                	  erl_syntax:atom(io_lib),
                	  erl_syntax:atom(format),
                	  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                	 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                	 none,
                	 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    {{CallAst, #ast_info{}}, TreeWalker1}.


%% @doc Generate an image tag based on the image name and the arguments
%% Author: Marc Worrell
%% @todo Optimization for the situation where all parameters are constants
image_ast(FilenameValue, Args, Context, TreeWalker) ->
    FilenameAst = resolve_value_ast(FilenameValue, Context, TreeWalker),
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                        erl_syntax:atom(z_media_tag),
                        erl_syntax:atom(tag),
                        [   FilenameAst,
                            ArgsAst,
                            z_context_ast(Context)
                        ]
                    ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
                      none,
                      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                	  erl_syntax:atom(io_lib),
                	  erl_syntax:atom(format),
                	  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                	 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                	 none,
                	 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    {{CallAst, #ast_info{}}, TreeWalker1}.


%% @doc Generate an image url based on the image name and the arguments
%% Author: Marc Worrell
%% @todo Optimization for the situation where all parameters are constants
image_url_ast(FilenameValue, Args, Context, TreeWalker) ->
    FilenameAst = resolve_value_ast(FilenameValue, Context, TreeWalker),
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                        erl_syntax:atom(z_media_tag),
                        erl_syntax:atom(url),
                        [   FilenameAst,
                            ArgsAst,
                            z_context_ast(Context)
                        ]
                    ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
                      none,
                      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                	  erl_syntax:atom(io_lib),
                	  erl_syntax:atom(format),
                	  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                	 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                	 none,
                	 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    {{CallAst, #ast_info{}}, TreeWalker1}.
    

%% Added by Marc Worrell - handle url generation using the url patterns
url_ast(Name, Args, Context, TreeWalker) ->
    % Check if the 'escape' argument is there
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(z_dispatcher),
                erl_syntax:atom(url_for),
                [   erl_syntax:atom(Name), 
                    ArgsAst,
                    z_context_ast(Context)
                ]
            ),
    {{AppAst, #ast_info{}}, TreeWalker1}.  


print_ast(Value, Context, TreeWalker) ->
    ValueAst = resolve_value_ast(Value, Context, TreeWalker),
    PrintAst = erl_syntax:application(
                erl_syntax:atom(io_lib),
                erl_syntax:atom(format),
                [   erl_syntax:string("~p"), 
                    erl_syntax:list([ValueAst])
                ]
            ),
    FlattenAst = erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(flatten),
                [PrintAst]
            ),
    EscapeAst = erl_syntax:application(
                  erl_syntax:atom(mochiweb_html),
                  erl_syntax:atom(escape),
                  [FlattenAst]
            ),
    PreAst = erl_syntax:list([
                erl_syntax:string("<pre>"),
                EscapeAst,
                erl_syntax:string("</pre>")
            ]),
    {{PreAst, #ast_info{}}, TreeWalker}. 


lib_ast(LibList, Args, Context, TreeWalker) ->
    Libs = [ unescape_string_literal(V) || {string_literal, _, V} <- LibList ],
    LibsAst = erl_syntax:list([ erl_syntax:string(L) || L <- Libs ]),
    {ArgList, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    Ast = erl_syntax:application(
                erl_syntax:atom(z_lib_include),
                erl_syntax:atom(tag),
                [   LibsAst,
		    ArgList,
                    z_context_ast(Context)
                ]),
    {{Ast, #ast_info{}}, TreeWalker1}.


cache_ast(MaxAge, Args, Body, Context, TreeWalker) ->
	{Name, Args1} = case Args of
		[{{identifier, _, Ident}, true}|RestArgs] when Ident =/= "if_anonymous" -> {Ident, RestArgs};
		_ -> {z_ids:id(), Args}
	end,
	MaxAge1 = case MaxAge of
		{number_literal, _, Value} -> list_to_integer(Value);
		undefined -> 0
	end,
    {ArgsAst, ArgsTreeWalker} = scomp_ast_list_args(Args1, Context, TreeWalker),
    ContextVarAst = erl_syntax:variable("Ctx_" ++ z_ids:identifier(10)),
    {{BodyAst, BodyInfo}, BodyTreeWalker} = body_ast(
                                                Body, 
                                                Context#dtl_context{local_scopes = [ [{'ZpContext', ContextVarAst}] | Context#dtl_context.local_scopes ]},
                                                ArgsTreeWalker),
    FuncAst = erl_syntax:fun_expr([
        erl_syntax:clause(
            [ ContextVarAst ], 
            none, 
            [ BodyAst ]
        )
    ]),
    CacheAst = erl_syntax:application(
                  erl_syntax:atom(erlydtl_runtime),
                  erl_syntax:atom(cache),
                  [ erl_syntax:integer(MaxAge1),
                    erl_syntax:atom(list_to_atom("$tpl$" ++ Name)),
                    ArgsAst,
                    FuncAst,
                    z_context_ast(Context)]
            ),
    {{CacheAst, BodyInfo}, BodyTreeWalker}.


resolve_value_ast(Value, Context, TreeWalker) ->
    {{Ast,_Info},_TreeWalker} = value_ast(Value, false, Context, TreeWalker),
    Ast.


%% Added by Marc Worrell - handle evaluation of scomps by z_scomp
scomp_ast(ScompName, Args, false = _All, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(z_scomp),
                erl_syntax:atom(render),
                [   erl_syntax:atom(ScompName), 
                    ArgsAst,
                    erl_syntax:variable("Variables"),
                    z_context_ast(Context)
                ]
            ),
    RenderedAst = erl_syntax:variable("Rendered"),
    CleanedAst = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(prune_for_template),
                [RenderedAst]
            ),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
                      none,
                      [CleanedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                	  erl_syntax:atom(io_lib),
                	  erl_syntax:atom(format),
                	  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                	 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                	 none,
                	 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    {{CallAst, #ast_info{}}, TreeWalker1};
scomp_ast(ScompName, Args, true, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(z_scomp),
                erl_syntax:atom(render_all),
                [   erl_syntax:atom(ScompName), 
                    ArgsAst,
                    erl_syntax:variable("Variables"),
                    z_context_ast(Context)
                ]
            ),
    {{AppAst, #ast_info{}}, TreeWalker1}.


scomp_ast_list_args(Args, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1}= interpreted_args(Args, Context, TreeWalker),
    PropListAst = [ erl_syntax:tuple([erl_syntax:atom(A), B]) || {A,B} <- ArgsAst ],
    { erl_syntax:list(PropListAst), TreeWalker1}.


%%  lists:append(AutoId,"-Name")
auto_id_ast({identifier, _, Name}, Context, TreeWalker) ->
    {{   erl_syntax:application(
                    erl_syntax:atom(lists), erl_syntax:atom(append),
                    [resolve_scoped_variable_ast("$autoid", Context), erl_syntax:string([$-|Name])]),
        #ast_info{}
    }, TreeWalker#treewalker{has_auto_id=true}};

auto_id_ast({{identifier, _, Name}, {identifier, _, _} = Var}, Context, TreeWalker) ->
    {{V, _, VarInfo}, TreeWalker1} = resolve_variable_ast({variable, Var}, Context, TreeWalker),
    {{   erl_syntax:application(
                    erl_syntax:atom(lists), erl_syntax:atom(append),
                    [   
                        erl_syntax:list([
                            resolve_scoped_variable_ast("$autoid", Context), 
                            erl_syntax:string([$-|Name]++"-"),
                            erl_syntax:application(
                                erl_syntax:atom(z_convert),
                                erl_syntax:atom(to_list),
                                [V])
                        ])
                    ]),
       VarInfo
    }, TreeWalker1#treewalker{has_auto_id=true}}.


interpreted_args(Args, Context, TreeWalker) ->
    lists:foldr(
        fun
            ({{identifier, _, "postback"}, {Literal, _, Value}}, {Acc, TW}) when Literal == string_literal; Literal == trans_literal ->
                % string postbacks are always translated to atoms
                { [ {list_to_atom("postback"), erl_syntax:atom(unescape_string_literal(Value))} | Acc ], TW };
            ({{identifier, _, Key}, Value}, {Acc, TW}) ->
                % a normal key=value argument
                {Ast, TW1} = interpreted_argval(Value, Context, TW),
                { [ {list_to_atom(Key), Ast} | Acc ], TW1 }
        end,
        {[], TreeWalker},
        Args).

interpreted_argval({number_literal, _, Value}, _Context, TreeWalker) -> 
    {erl_syntax:integer(list_to_integer(Value)), TreeWalker};
interpreted_argval({string_literal, _, Value}, _Context, TreeWalker) -> 
    {erl_syntax:string(unescape_string_literal(Value)), TreeWalker};
interpreted_argval({atom_literal, _, Value}, _Context, TreeWalker) -> 
    {erl_syntax:atom(to_atom(unescape_string_literal(Value))), TreeWalker};
interpreted_argval({trans_literal, _, Value}, Context, TreeWalker) ->
    {trans_literal_ast(Value, Context), TreeWalker};
interpreted_argval({auto_id, Name}, Context, TreeWalker) ->
    {{V, _}, TreeWalker1} = auto_id_ast(Name, Context, TreeWalker), 
    {V, TreeWalker1};
interpreted_argval({tuple_value, {identifier, _, TupleName}, TupleArgs}, Context, TreeWalker) ->
    {ArgList, TreeWalker1} = scomp_ast_list_args(TupleArgs, Context, TreeWalker),
    {erl_syntax:tuple([erl_syntax:atom(TupleName), ArgList]), TreeWalker1};
interpreted_argval({value_list, Values}, Context, TreeWalker) ->
    {List, TreeWalker1} = lists:foldr(
        fun(V, {Acc, TW}) -> 
            {VAst, TW1} = interpreted_argval(V, Context, TW),
            {[VAst|Acc], TW1}
        end,
        {[], TreeWalker},
        Values
    ),
    {erl_syntax:list(List), TreeWalker1};
interpreted_argval(true, _Context, TreeWalker) ->
    {erl_syntax:atom(true), TreeWalker};
interpreted_argval(Value, Context, TreeWalker) ->
    {{Ast, _VarName, _VarInfo}, TreeWalker1} = resolve_variable_ast(Value, Context, TreeWalker),
    {Ast, TreeWalker1}.



notify(_Msg, #context{host=test}) ->
    nop;
notify(Msg, ZContext) ->
    z_notifier:notify(Msg, ZContext).

