%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

-module(eunit_tty).

-include("exmpp_xml.hrl").

-export([
  start/1,
  start/2
]).

-record(state, {
  mode,
  succeed = 0,
  fail = 0,
  abort = 0,
  skip = 0,
  data,
  parent_state
}).

% --------------------------------------------------------------------
% Initialization.
% --------------------------------------------------------------------

%% @spec (List) -> Pid
%%     List = eunit_data:testInfoList()
%%     Pid = pid()
%% @doc Start the reporter.

start(List) ->
    start(List, []).

%% @spec (List, Options) -> Pid
%%     List = eunit_data:testInfoList()
%%     Options = [Option]
%%     Option = {mode, junit}
%%     Pid = pid()
%% @doc Start the reporter.
%%
%% With option `junit', the reporter will write JUnit XML file(s).

start(List, Options) ->
    St = #state{mode = proplists:get_value(mode, Options, single)},
    Id = [],
    spawn(fun () -> init(Id, List, St) end).

init(Id, List, St0) ->
    receive
	{start, Reference} ->
	    St = group_begin(Id, "", List, St0),
	    receive
		{stop, Reference, Reply_To} ->
		    Result = case St0#state.mode of
                        single ->
                            if
                                St#state.fail == 0,
                                St#state.abort == 0,
                                St#state.succeed > 0 ->
                                    ok;
                                St#state.fail == 0,
                                St#state.abort == 0,
                                St#state.succeed == 0 ->
                                    skipped;
                                true ->
                                    error
                            end;
                        junit ->
                            ok;
                        _ ->
                            error
                    end,
		    Reply_To ! {result, Reference, Result},
		    ok
            end
    end.

% --------------------------------------------------------------------
% Group handling.
% --------------------------------------------------------------------

group_begin(Id, Desc, Es, St) ->
    St0 = save_parent_state(St),
    St1 = print_group_start(St0, Id, Desc),
    Final_St = case wait(Id, St1) of
	{{progress, 'begin', group}, St2} ->
	    group_end(Id, Desc, Es, tests(Es, St2));
	{{cancel, Reason}, St2} ->
            Size = eunit_data:list_size(Es),
	    St3 = St2#state{skip = St2#state.skip + Size},
            print_group_cancel(St3, Id, Desc, Reason)
    end,
    restore_parent_state(Final_St).

group_end(Id, Desc, Es, St) ->
    case wait(Id, St) of
        {{progress, 'end', {Count, Time, Output}}, St1} ->
            if
                Id /= [] ->
                    print_group_end(St1, Id, Desc, Count, Time, Output);
                true ->
                    print_build_errors(St1, Id, Desc, Count, Time, Output)
            end;
        {{cancel, undefined}, St1} ->
            St1;  % "skipped" message is not interesting here.
        {{cancel, Reason}, St1} ->
            Size = eunit_data:list_size(Es),
	    St2 = St1#state{abort = St1#state.abort + Size},
            print_group_cancel(St2, Id, Desc, Reason)
    end.

% --------------------------------------------------------------------
% Test handling.
% --------------------------------------------------------------------

test_begin(Id, Desc, {Module, Name}, St) ->
    test_begin(Id, Desc, {Module, Name, 0}, St);
test_begin(Id, Desc, {Module, Name, Line}, St) ->
    Text = format_test_name(Module, Name, Line, Desc),
    St1 = print_test_start(St, Id, Text),
    case wait(Id, St1) of
	{{progress, 'begin', test}, St2} ->
            % Test in progress.
	    test_end(Id, Text, St2);
	{{cancel, Reason}, St2} ->
            % Test skipped.
            St3 = St2#state{skip = St2#state.skip + 1},
            print_test_skip(St3, Id, Text, Reason)
    end.

test_end(Id, Text, St) ->
    case wait(Id, St) of
	{{progress, 'end', {Result, Time, Output}}, St1} ->
            % Test done.
            if
                Result == ok ->
                    St2 = St1#state{succeed = St1#state.succeed + 1},
                    print_test_success(St2, Id, Text, Time, Output);
                true ->
                    St2 = St1#state{fail = St1#state.fail + 1},
                    print_test_error(St2, Id, Text, Result, Time, Output)
            end;
	{{cancel, Reason}, St1} ->
            % Test aborted.
	    St2 = St1#state{abort = St1#state.abort + 1},
            print_test_abort(St2, Id, Text, Reason)
    end.

% --------------------------------------------------------------------
% Mode-dependent reporting functions.
% --------------------------------------------------------------------

print_group_start(#state{mode = junit} = St, _Id, _Desc) ->
    St#state{data = []};
print_group_start(St, _Id, _Desc) ->
    St.

print_group_end(#state{mode = junit, data = []} = St, _Id, _Desc,
  _Count, _Time, _Output) ->
    St;
print_group_end(#state{mode = junit} = St, _Id, "", _Count, _Time, _Output) ->
    Parent_St = St#state.parent_state,
    Parent_St1 = Parent_St#state{
      succeed = Parent_St#state.succeed + St#state.succeed,
      fail = Parent_St#state.fail + St#state.fail,
      abort = Parent_St#state.abort + St#state.abort,
      skip = Parent_St#state.skip + St#state.skip,
      data = lists:flatten([St#state.data, Parent_St#state.data])
    },
    St#state{parent_state = Parent_St1};
print_group_end(#state{mode = junit} = St, Id, Desc, _Count, Time, _Output) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', Desc),
    Attrs1 = exmpp_xml:set_attribute_in_list(Attrs0, 'time',
      io_lib:format("~.6f", [Time / 1000])),
    Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'tests',
      St#state.succeed + St#state.fail + St#state.abort + St#state.skip),
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2, 'errors',
      St#state.fail),
    Attrs4 = exmpp_xml:set_attribute_in_list(Attrs3, 'failures',
      St#state.abort),
    Attrs5 = exmpp_xml:set_attribute_in_list(Attrs4, 'skipped',
      St#state.skip),
    Testsuite = exmpp_xml:set_children(
      #xmlel{name = 'testsuite', attrs = Attrs5},
      lists:reverse(St#state.data)
    ),
    % Write XML file.
    Filename = junit_report_filename(Id, Desc),
    Content = exmpp_xml:document_to_list(
      exmpp_xml:indent_document(Testsuite, <<"  ">>)),
    file:write_file(Filename, Content),
    St;
print_group_end(St, _Id, _Desc, _Count, _Time, _Output) ->
    St.

print_group_cancel(St, [], _Desc, _Reason) ->
    St;
print_group_cancel(St, _Id, Desc, Reason) ->
    io:format("~s:~n~p~n", [Desc, Reason]),
    St.

print_build_errors(#state{mode = junit, data = []} = St, _Id, _Desc,
  _Count, _Time, _Output) ->
    St;
print_build_errors(#state{mode = junit} = St, _Id, Desc,
  _Count, Time, _Output) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', "Build errors"),
    Attrs1 = exmpp_xml:set_attribute_in_list(Attrs0, 'time',
      io_lib:format("~.6f", [Time / 1000])),
    Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'tests',
      St#state.succeed + St#state.fail + St#state.abort + St#state.skip),
    Attrs3 = exmpp_xml:set_attribute_in_list(Attrs2, 'errors',
      St#state.fail),
    Attrs4 = exmpp_xml:set_attribute_in_list(Attrs3, 'failures',
      St#state.abort),
    Attrs5 = exmpp_xml:set_attribute_in_list(Attrs4, 'skipped',
      St#state.skip),
    Testsuite = exmpp_xml:set_children(
      #xmlel{name = 'testsuite', attrs = Attrs5},
      lists:reverse(St#state.data)
    ),
    % Write XML file.
    Filename = junit_report_filename(build, Desc),
    Content = exmpp_xml:document_to_list(
      exmpp_xml:indent_document(Testsuite, <<"  ">>)),
    file:write_file(Filename, Content),
    St;
print_build_errors(St, _Id, _Desc, _Count, _Time, _Output) ->
    St.

print_test_start(St, _Id, _Desc) ->
    St.

print_test_success(#state{mode = junit, data = XML} = St, _Id, Desc, Time,
  _Output) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', Desc),
    Attrs1 = exmpp_xml:set_attribute_in_list(Attrs0, 'time',
      io_lib:format("~.6f", [Time / 1000])),
    Testcase = #xmlel{name = 'testcase', attrs = Attrs1},
    New_XML = [Testcase | XML],
    St#state{data = New_XML};
print_test_success(St, _Id, _Desc, _Time, _Output) ->
    St.

print_test_skip(#state{mode = junit, data = XML} = St, _Id, Desc, Reason) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', Desc),
    Skipped = exmpp_xml:set_cdata(#xmlel{name = 'skipped'},
      lists:flatten(io_lib:format("~p", [Reason]))),
    Testcase = exmpp_xml:set_children(
      #xmlel{name = 'testcase', attrs = Attrs0},
      [Skipped]
    ),
    New_XML = [Testcase | XML],
    St#state{data = New_XML};
print_test_skip(St, _Id, Desc, Reason) ->
    io:format("~n~s (skip):~n~s~n", [Desc, format_test_error(Reason)]),
    St.

print_test_error(#state{mode = junit, data = XML} = St, _Id, Desc,
  Result, Time, _Output) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', Desc),
    Attrs1 = exmpp_xml:set_attribute_in_list(Attrs0, 'time',
      io_lib:format("~.6f", [Time / 1000])),
    Error = exmpp_xml:set_cdata(#xmlel{name = 'error'},
      format_test_error(Result)),
    Testcase = exmpp_xml:set_children(
      #xmlel{name = 'testcase', attrs = Attrs1},
      [Error]
    ),
    New_XML = [Testcase | XML],
    St#state{data = New_XML};
print_test_error(St, _Id, Desc, Result, _Time, _Output) ->
    io:format("~n~s:~n~s~n", [Desc, format_test_error(Result)]),
    St.

print_test_abort(#state{mode = junit, data = XML} = St, _Id, Desc, Reason) ->
    Attrs0 = exmpp_xml:set_attribute_in_list([], 'name', Desc),
    Failure = exmpp_xml:set_cdata(#xmlel{name = 'failure'},
      lists:flatten(io_lib:format("~p", [Reason]))),
    Testcase = exmpp_xml:set_children(
      #xmlel{name = 'testcase', attrs = Attrs0},
      [Failure]
    ),
    New_XML = [Testcase | XML],
    St#state{data = New_XML};
print_test_abort(St, _Id, Desc, Reason) ->
    io:format("~n~s (abort):~n~s~n", [Desc, format_test_error(Reason)]),
    St.

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

wait(Id, St) ->
    receive
        {status, Id, Data} -> {Data, St}
    end.

tests([E | Es], St) ->
    tests(Es, entry(E, St));
tests([], St) ->
    St.

entry({item, Id, Desc, Test}, St) ->
    test_begin(Id, Desc, Test, St);
entry({group, Id, Desc, Es}, St) ->
    group_begin(Id, Desc, Es, St).

format_test_name(Module, Name, Line, "") ->
    L = if
        Line == 0 -> "";
        true      -> io_lib:format("~b:", [Line])
    end,
    io_lib:format("~s:~s~s", [Module, L, Name]);
format_test_name(erl_eval, expr, _, Desc) ->
    Desc;
format_test_name(Module, Name, Line, Desc) ->
    M = format_test_name(Module, Name, Line, ""),
    io_lib:format("~s (~s)", [M, Desc]).

% Format test error.
format_test_error({skipped, undefined}) ->
    "skipped";
format_test_error({skipped, timeout}) ->
    "time out";
format_test_error({skipped, {startup, Reason}}) ->
    lists:flatten(io_lib:format("could not start test process:~n~P",
        [Reason, 15]));
format_test_error({skipped, {blame, _Sub_Id}}) ->
    "cancelled because of subtask";
format_test_error({skipped, {exit, Reason}}) ->
    lists:flatten(io_lib:format("unexpected termination of test process:~n~P",
        [Reason, 15]));
format_test_error({skipped, {abort, Reason}}) ->
    eunit_lib:format_error(Reason);
format_test_error({error, {error, {Type, Props}, _}}) ->
    Type_For_Human = case Type of
        assertion_failed   -> "Test not true";
        assertNot_failed   -> "Test not false";
        assertMatch_failed -> "Value doesn't match";
        assertThrow_failed -> "Exception doesn't match";
        _                  -> atom_to_list(Type)
    end,
    Expression = proplists:get_value(expression, Props),
    Expected = proplists:get_value(expected, Props),
    Value = proplists:get_value(value, Props),
    Unexpected_Exception = proplists:get_value(unexpected_exception, Props),
    Output0 = io_lib:format(
      "~s:~n"
      "  Expression:~n"
      "    ~s~n"
      "  Expected:~n"
      "    ~s~n",
      [beautify_expr(Type_For_Human), beautify_expr(Expression),
        beautify_expr(Expected)]),
    Output1 = case Value of
        undefined ->
            Output0;
        _ ->
            Output0 ++ io_lib:format(
              "  Value:~n"
              "    ~s~n", [beautify_expr(Value)])
    end,
    Output2 = case Unexpected_Exception of
        undefined ->
            Output1;
        _ ->
            Output1 ++ io_lib:format(
              "  Unexpected exception:~n"
              "    ~s~n", [beautify_expr(Unexpected_Exception)])
    end,
    lists:flatten(Output2);
format_test_error({error, Exception}) ->
    eunit_lib:format_exception(Exception).

%beautify_expr({Type, Exception, _Stacktrace})
%  when Type == error; Type == throw ->
%    atom_to_list(Type) ++ ":" ++ beautify_expr(Exception);
beautify_expr(Expr) ->
    Expr1 = case io_lib:deep_char_list(Expr) of
        true  -> Expr;
        false -> lists:flatten(io_lib:format("~p", [Expr]))
    end,
    Tokens = string:tokens(Expr1, ","),
    Fun = fun(S) -> string:join(string:tokens(S, " "), "") end,
    New_Tokens = lists:map(Fun, Tokens),
    string:join(New_Tokens, ", ").

save_parent_state(#state{mode = junit} = St) ->
    #state{mode = St#state.mode, parent_state = St};
save_parent_state(St) ->
    St.

restore_parent_state(#state{mode = junit, parent_state = St}) ->
    St;
restore_parent_state(St) ->
    St.

junit_report_filename(build, _Desc) ->
    "exmpp-junit-build.xml";
junit_report_filename(Id, "") when is_list(Id) ->
    "exmpp-junit" ++ junit_report_filename2(Id, "") ++ ".xml";
junit_report_filename(Id, Desc) when is_list(Id) ->
    "exmpp-junit" ++ junit_report_filename2(Id, "") ++
        escape_desc(Desc, "-", true) ++ ".xml".

junit_report_filename2([I | Rest], Suffix) ->
    junit_report_filename2(Rest, Suffix ++ "-" ++ integer_to_list(I));
junit_report_filename2([], Suffix) ->
    Suffix.

escape_desc([C | Rest], Result, _Sep_Added) when
  C >= $a, C =< $z;
  C >= $A, C =< $Z;
  C >= $0, C =< $9;
  C == $_; C == $- ->
    escape_desc(Rest, [C | Result], false);
escape_desc([_C], Result, Sep_Added) ->
    escape_desc([], Result, Sep_Added);
escape_desc([_C | Rest], Result, false) ->
    escape_desc(Rest, [$- | Result], true);
escape_desc([_C | Rest], Result, true) ->
    escape_desc(Rest, Result, true);
escape_desc([], Result, _Sep_Added) ->
    lists:reverse(Result).
