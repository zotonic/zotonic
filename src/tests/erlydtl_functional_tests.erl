%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc       ErlyDTL test suite
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
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
%%% @since 2008-02-11 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_functional_tests).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


%% API
-export([run_tests/0, run_test/1, find_file/1, find_file/2]).

-export([render/3]).

test_list() ->
% order is important.
    [   "inherit", "autoescape", "comment", "extends", "overrules",
        "filters", 
        "for", "for_list", "for_tuple", "for_records",
        "include", 
        "if", "ifequal", "ifnotequal", 
        %"now",
        "var", "cycle", 
        "custom_tag", 
        % "custom_tag_error",
        "block_recurse_error",
        "custom_call",
        "with_multiple"
    ].

% Not supported for now (due to compile vars)
% "for_list_preset", "for_preset", "for_records_preset", "if_preset", "ifnotequal_preset", 
% "ifequal_preset", "var_preset", 

setup_compile("for_list_preset") ->
    CompileVars = [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}],
    {ok, CompileVars};
setup_compile("for_preset") ->
    CompileVars = [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}],
    {ok, CompileVars};
setup_compile("for_records_preset") ->
    Link1a = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2a = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3a = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    CompileVars = [{software_links, [Link1a, Link2a, Link3a]}], 
    {ok, CompileVars};
setup_compile("if_preset") ->
    CompileVars = [{var1, "something"}],
    {ok, CompileVars};
setup_compile("ifequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, CompileVars};
setup_compile("ifnotequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, CompileVars};
setup_compile("var_preset") ->
    CompileVars = [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}],
    {ok, CompileVars};
setup_compile("custom_tag_error") ->
    CompileVars  = [],
    {error, CompileVars};
setup_compile("block_recurse_error") ->
    CompileVars  = [],
    {error, CompileVars};
setup_compile(_) ->
    {ok, []}.

%% @spec (Name::string()) -> {CompileStatus::atom(), PresetVars::list(), 
%%     RenderStatus::atom(), RenderVars::list()} | skip
%% @doc
%% @end 
%%--------------------------------------------------------------------
setup("autoescape") ->
    RenderVars = [{var1, "<b>bold</b>"}],
    {ok, RenderVars};  
setup("extends") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("filters") ->
    RenderVars = [
        {date_var1, {1975,7,24}},
        {datetime_var1, {{1975,7,24}, {7,13,1}}},
        {'list', ["eins", "zwei", "drei"]}
    ],
    {ok, RenderVars};
setup("for") ->
    RenderVars = [{fruit_list, ["apple", "banana", "coconut"]}],
    {ok, RenderVars};
setup("for_list") ->
    RenderVars = [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}],
    {ok, RenderVars};
setup("for_tuple") ->
    RenderVars = [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}],
    {ok, RenderVars};
setup("for_records") ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    RenderVars = [{link_list, [Link1, Link2, Link3]}],
    {ok, RenderVars};  
setup("for_records_preset") ->
    Link1b = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2b = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3b = [{name, "Nikon"}, {url, "http://nikon.com"}],
    RenderVars = [{photo_links, [Link1b, Link2b, Link3b]}],
    {ok, RenderVars};
setup("include") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("if") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars}; 
setup("ifequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};      
setup("ifequal_preset") ->
    RenderVars = [{var3, "bar"}],
    {ok, RenderVars};   
setup("ifnotequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};        
setup("var") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}],
    {ok, RenderVars};
setup("var_preset") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars}; 
setup("cycle") ->
    RenderVars = [{test, [integer_to_list(X) || X <- lists:seq(1, 20)]},
                  {a, "Apple"}, {b, "Banana"}, {c, "Cherry"}],
    {ok, RenderVars};
setup("include_template") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("include_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path2") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};



%%--------------------------------------------------------------------       
%% Custom tags
%%--------------------------------------------------------------------
setup("custom_tag_error") ->
    RenderVars = [],
    {skip, RenderVars};        
setup("custom_call") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};    

setup(_) ->
    {ok, []}.
    

run_tests() ->    
    io:format("Running functional tests...~n"),
    case fold_tests() of
        {N, []}->
            Msg = lists:concat(["All ", N, " functional tests passed~n~n"]),
            io:format(Msg),
            {ok, Msg};
        {_, Errs} ->
            io:format("Errors: ~p~n~n",[Errs]),
            failed
    end.


run_test(Name) ->
    test_compile_render(filename:join([templates_docroot(), Name])).


%%====================================================================
%% Internal functions
%%====================================================================

fold_tests() ->
    lists:foldl(fun(Name, {AccCount, AccErrs}) ->
                case test_compile_render(Name) of
                    ok -> 
                        {AccCount + 1, AccErrs};
                    {error, Reason} -> 
                        {AccCount + 1, [{Name, Reason} | AccErrs]}
                end
        end, {0, []}, test_list()
    ).

test_compile_render(Name) ->  
    File = filename:join([templates_docroot(), Name]),
    Module = "example_" ++ Name,
    case setup_compile(Name) of
        {CompileStatus, CompileVars} ->
            Options = [
                {vars, CompileVars}, 
                {force_recompile, true},
                {finder, {?MODULE,find_file}}],
            io:format(" Template: ~p, ... compiling ... ", [Name]),
            case catch erlydtl:compile(File, Name, Module, Options, context()) of
                {ok, ModuleName} ->
                    case CompileStatus of
                        ok -> test_render(Name, ModuleName);
                        _ -> io:format("~n"), {error, "compiling should have failed :" ++ File}
                    end;
                {error, Err} ->
                    case CompileStatus of
                        error ->
                            io:format("~n"),  
                            ok;
                        _ ->
                            io:format("~nCompile errror: ~p~n",[lists:flatten(Err)]), 
                            Err
                    end
            end;
        skip ->
            ok;
        _ ->
            {error, "no 'setup' clause defined for this test"}
    end.


test_render(Name, Module) ->
    File = filename:join([templates_docroot(), Name]),
    {RenderStatus, Vars} = setup(Name),
    case catch Module:render(Vars, context()) of
        {ok, Data} ->
            io:format("rendering~n"), 
            case RenderStatus of
                ok ->
                    File = Module:source(),
                    OutFile = filename:join([templates_outdir(), filename:basename(File)]),
                    case file:open(OutFile, [write]) of
                        {ok, IoDev} ->
                            file:write(IoDev, Data),
                            file:close(IoDev),

                            ExpectedFile = filename:join([templates_expectdir(), filename:basename(File)]),
                            case file:read_file(ExpectedFile) of
                                {ok, Expected} ->
                                    case iolist_to_binary(Data) of
                                        Expected -> ok;
                                        _Other -> {error, "Does not match expected output"}
                                    end;
                                {error, _} ->
                                    io:format(" NO FILE WITH EXPECTED OUTPUT (~p).~n", [ExpectedFile]),
                                    ok
                            end;
                        Err ->
                            {error, {file, OutFile, Err}}
                    end;
                _ ->
                    {error, "rendering should have failed :" ++ File}
            end;
        {'EXIT', E} ->
            io:format("~n"),
            {error, {"failed invoking render method: " ++ z_convert:to_list(Module), E}};
        Err ->
            io:format("~n"),
            case RenderStatus of
                error ->  ok;
                _ -> Err
            end
    end.   

find_file(File) ->
    hd(find_file(File, true)).

find_file(File, false) ->
    DocRoot = templates_docroot(),
    DocRoot2 = templates_docroot2(),
    case lists:prefix(DocRoot, File) or lists:prefix(DocRoot2, File) of
        true -> [File];
        false -> [filename:join([DocRoot, File])]
    end;
find_file(File, true) ->
    DocRoot = templates_docroot(),
    DocRoot2 = templates_docroot2(),
    case lists:prefix(DocRoot, File) or lists:prefix(DocRoot2, File) of
        true -> 
            [File];
        false -> 
            lists:filter(fun(F) -> filelib:is_file(F) end, 
                        [ filename:join([DocRoot, File]),
                          filename:join([DocRoot2, File]) ])
    end.

templates_docroot() ->
    filename:join([erlydtl_deps:get_base_dir(), "src", "tests", "erlydtl", "docroot"]).

templates_docroot2() ->
    filename:join([erlydtl_deps:get_base_dir(), "src", "tests", "erlydtl", "docroot2"]).

templates_outdir() ->   
    Dir = filename:join([erlydtl_deps:get_base_dir(), "src", "tests", "erlydtl", "rendered_output"]),
    ok = filelib:ensure_dir(filename:join([Dir,"test"])),
    Dir.

templates_expectdir() ->   
    filename:join([erlydtl_deps:get_base_dir(), "src", "tests", "erlydtl", "expected_output"]).


context() ->
    z_context:new_tests().

%% Used by the test 'custom_call'
render(undefined, Vars, _Context) ->
    {ok, "<<undefined>>-" ++ proplists:get_value(var1, Vars, "") ++ "-ok."};
render(Arg, Vars, _Context) ->
    {ok, Arg ++ "-" ++ proplists:get_value(var1, Vars, "") ++ "-ok."}.
    