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

-module(check_coverity).

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    {ok, [[Top_Srcdir]]} = init:get_argument(top_srcdir),
    {ok, [Tests]} = init:get_argument(tests),
    {ok, [Covered_Modules]} = init:get_argument(covered_modules),
    cover:start(),
    cover:compile_directory(filename:join([Top_Srcdir, "src", "core"]), [
        {i, filename:join(Top_Srcdir, "include")}]),
    cover:compile_directory(filename:join([Top_Srcdir, "src", "server"]), [
        {i, filename:join(Top_Srcdir, "include")}]),
    cover:compile_directory(filename:join([Top_Srcdir, "src", "client"]), [
        {i, filename:join(Top_Srcdir, "include")}]),
    try
        run_tests(Tests),
        print_coverage(Covered_Modules),
        cover:stop(),
        ok
    catch
        throw:_Exception ->
            testsuite:skip()
    end.

run_tests([Test | Rest]) ->
    Mod = list_to_atom(Test),
    Mod:do_check(),
    run_tests(Rest);
run_tests([]) ->
    ok.

print_coverage(Modules) ->
    io:format("Coverage:~n"),
    print_coverage2(Modules).

print_coverage2([Module | Rest]) ->
    Mod = list_to_atom(Module),
    {ok, {_Module, {Cov, Not_Cov}}} = cover:analyse(Mod, module),
    if
        Cov > 0; Not_Cov > 0 ->
            file:write_file("cover_" ++ Module ++ ".percent",
              list_to_binary(io_lib:format("~.1f",
                  [Cov * 100 / (Cov + Not_Cov)]))),
            io:format("- ~s: ~.1f%~n",
              [Mod, Cov * 100 / (Cov + Not_Cov)]);
        true ->
            io:format("- ~s: n/a~n~n", [Mod])
    end,
    cover:analyse_to_file(Mod, "cover_" ++ Module ++ ".out", []),
    print_coverage2(Rest);
print_coverage2([]) ->
    ok.
