%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for development editor discovery and launching.
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

-module(z_development_editor_tests).
-include_lib("eunit/include/eunit.hrl").

preference_order_test() ->
    Config = #{id => configured, rank => 1, available => false},
    Session = #{id => vim, rank => 3},
    Association = #{id => sublime, rank => 4},
    Running = #{id => code, rank => 5},
    Installed = #{id => zed, rank => 6},
    ?assertEqual(Config, z_development_editor:select([Running, Config, Session])),
    ?assertEqual(Session, z_development_editor:select([Installed, Association, Session])),
    ?assertEqual(Association, z_development_editor:select([Running, Association])),
    ?assertEqual(undefined, z_development_editor:select([Running, #{id => other, rank => 5}])),
    ?assertEqual(undefined, z_development_editor:select([])).

quoted_command_test() ->
    ?assertEqual({ok, ["/Applications/My Editor/bin/edit", "--goto", "{file}:{line}"]},
        z_development_editor:split_command("'/Applications/My Editor/bin/edit' --goto \"{file}:{line}\"")),
    ?assertEqual({ok, ["editor", "$(touch", "/tmp/never)"]},
        z_development_editor:split_command("editor $(touch /tmp/never)")),
    ?assertEqual({error, invalid_command}, z_development_editor:split_command("editor 'unfinished")).

literal_filename_test() ->
    File = unicode:characters_to_binary("/tmp/héllo {line} $(command); 'file'.tpl"),
    ?assertEqual([unicode:characters_to_list(File) ++ ":42:3"],
        z_development_editor:arguments(#{args => ["{file}:{line}:{column}"]}, File, 42, 3)).

container_precedes_configuration_test() ->
    Saved = application:get_env(zotonic, development_editor_browser_only),
    try
        application:set_env(zotonic, development_editor_browser_only, true),
        ?assertEqual(#{browser_only => true, candidates => [], selected => undefined},
            z_development_editor:detect("/nonexistent.tpl")),
        ?assertEqual({error, browser_only}, z_development_editor:open("/nonexistent.tpl", 1, 1, <<"configured">>)),
        ?assertEqual({error, browser_only}, z_development_editor:edit(?MODULE))
    after restore(development_editor_browser_only, Saved)
    end.

%% Exercise real exec argument passing with a harmless launcher, not an editor.
launch_test_() ->
    {timeout, 30, fun() ->
        case z_development_editor:browser_only() of
            true -> ok;
            false -> launch_fixture()
        end
    end}.

launch_fixture() ->
    Dir = "/tmp/zotonic-editor-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Script = filename:join(Dir, "launcher with spaces"),
    Output = filename:join(Dir, "arguments"),
    Source = filename:join(Dir, "a héllo {line} $(never); `never` & 'quote'.tpl"),
    Saved = application:get_env(zotonic, development_editor),
    try
        ok = file:write_file(Source, <<"test">>),
        ok = file:write_file(Script, <<"#!/bin/sh\nout=$1\nshift\nprintf '%s\\n' \"$@\" > \"$out\"\n">>),
        ok = file:change_mode(Script, 8#700),
        application:set_env(zotonic, development_editor, {Script, [Output, "{file}", "{line}", "{column}"]}),
        ?assertEqual(ok, z_development_editor:open(Source, 42, 3, <<"auto">>)),
        ?assertEqual({ok, unicode:characters_to_binary(Source ++ "\n42\n3\n")}, file:read_file(Output)),
        ?assertEqual({error, choose_editor}, z_development_editor:open(Source, 1, 1, <<"not-a-candidate">>)),
        ?assertEqual(ok, z:edit(unicode:characters_to_binary(Source), 17)),
        ?assertEqual({ok, unicode:characters_to_binary(Source ++ "\n17\n1\n")}, file:read_file(Output)),
        ?assertEqual(ok, z:edit([{?MODULE, launch_fixture, 0, [{file, Source}, {line, 23}]}])),
        ?assertEqual({ok, unicode:characters_to_binary(Source ++ "\n23\n1\n")}, file:read_file(Output)),
        ?assertEqual(ok, z:edit(?MODULE)),
        ?assertEqual(ok, z:edit(?MODULE, launch_fixture)),
        {ok, FunctionOutput} = file:read_file(Output),
        [_, FunctionLine, <<"1">>, <<>>] = binary:split(FunctionOutput, <<"\n">>, [global]),
        ?assert(binary_to_integer(FunctionLine) > 1),
        ?assertEqual(ok, z:edit(?MODULE, {launch_fixture, 0})),
        ?assertEqual({ok, FunctionOutput}, file:read_file(Output)),
        ?assertEqual(ok, z:edit([{?MODULE, launch_fixture, []}])),
        ?assertEqual({ok, FunctionOutput}, file:read_file(Output)),
        ?assertEqual({error, function_not_found}, z:edit(?MODULE, absent_function)),
        ?assertEqual({error, badarg}, z:edit([]))
    after
        restore(development_editor, Saved),
        file:del_dir_r(Dir)
    end.

restore(Key, undefined) -> application:unset_env(zotonic, Key);
restore(Key, {ok, Value}) -> application:set_env(zotonic, Key, Value).

nvr_session_test_() ->
    {timeout, 30, fun() ->
        case z_development_editor:browser_only() of
            true -> ok;
            false -> nvr_fixture()
        end
    end}.

nvr_fixture() ->
    Dir = "/tmp/zotonic-nvr-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    SavedEnv = [{K, os:getenv(K)} || K <- ["PATH", "NVIM", "NVIM_LISTEN_ADDRESS", "VIM_SERVERNAME", "ZOTONIC_EDITOR", "VISUAL", "EDITOR"]],
    SavedConfig = application:get_env(zotonic, development_editor),
    try
        Nvr = filename:join(Dir, "nvr"),
        ok = file:write_file(Nvr, <<"#!/bin/sh\n[ \"$1\" = '--nostart' ] || exit 99\n[ \"$2\" = '--servername' ] || exit 98\n[ \"$3\" = '/tmp/test-nvim.sock' ] || exit 97\nprintf '1\\n'\n">>),
        ok = file:change_mode(Nvr, 8#700),
        application:unset_env(zotonic, development_editor),
        lists:foreach(fun({K, _}) -> os:unsetenv(K) end, SavedEnv),
        os:putenv("PATH", Dir),
        os:putenv("NVIM", "/tmp/test-nvim.sock"),
        #{selected := Selected} = z_development_editor:detect("/tmp/test.tpl"),
        ?assertMatch(#{id := <<"nvr">>, reason := inherited_session}, Selected),
        ?assertEqual(["--nostart", "--servername", "/tmp/test-nvim.sock", "--remote", "+call cursor(42,3)", "/tmp/test.tpl"],
            z_development_editor:arguments(Selected, "/tmp/test.tpl", 42, 3)),
        %% A stale server must not be selected just because the socket env is set.
        ok = file:write_file(Nvr, <<"#!/bin/sh\nexit 1\n">>),
        #{candidates := Candidates} = z_development_editor:detect("/tmp/test.tpl"),
        ?assertNot(lists:any(fun(C) -> maps:get(id, C) =:= <<"nvr">> end, Candidates))
    after
        restore(development_editor, SavedConfig),
        lists:foreach(fun({K, false}) -> os:unsetenv(K); ({K, V}) -> os:putenv(K, V) end, SavedEnv),
        file:del_dir_r(Dir)
    end.

association_fallback_test_() ->
    [{Extension, {timeout, 30, fun() ->
        case z_development_editor:browser_only() orelse element(1, os:type()) =:= win32 of
            true -> ok;
            false -> association_fixture(Extension)
        end
    end}} || Extension <- ["erl", "css"]].

association_fixture(Extension) ->
    Dir = "/tmp/zotonic-association-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    SavedEnv = [{K, os:getenv(K)} || K <- ["PATH", "NVIM", "NVIM_LISTEN_ADDRESS", "VIM_SERVERNAME", "ZOTONIC_EDITOR", "VISUAL", "EDITOR"]],
    SavedConfig = application:get_env(zotonic, development_editor),
    try
        %% Only the tested extension maps to Sublime; all others map to Word.
        Script = <<"#!/bin/sh\nfor last; do :; done\ncase \"$last\" in\n*.erl|application/x-erlang) printf 'sublime-text.desktop\\n';;\n*) printf 'word.desktop\\n';;\nesac\n">>,
        MimeScript = <<"#!/bin/sh\nif [ \"$2\" = filetype ]; then\ncase \"$3\" in *.erl) printf 'application/x-erlang\\n';; *) printf 'application/word\\n';; esac\nelse\ncase \"$3\" in application/x-erlang) printf 'sublime-text.desktop\\n';; *) printf 'word.desktop\\n';; esac\nfi\n">>,
        lists:foreach(fun({Name, Body}) ->
            Path = filename:join(Dir, Name),
            Probe = case Extension of
                "erl" -> Body;
                "css" -> binary:replace(
                            binary:replace(Body, <<".erl">>, <<".css">>, [global]),
                            <<"application/x-erlang">>, <<"text/css">>, [global])
            end,
            ok = file:write_file(Path, Probe),
            ok = file:change_mode(Path, 8#700)
        end, [{"osascript", Script}, {"xdg-mime", MimeScript}, {"subl", <<"#!/bin/sh\nexit 99\n">>}]),
        application:unset_env(zotonic, development_editor),
        lists:foreach(fun({K, _}) -> os:unsetenv(K) end, SavedEnv),
        os:putenv("PATH", Dir),
        #{selected := Selected} = z_development_editor:detect("/tmp/test.tpl"),
        ?assertMatch(#{id := <<"sublime">>, reason := file_association}, Selected)
    after
        restore(development_editor, SavedConfig),
        lists:foreach(fun({K, false}) -> os:unsetenv(K); ({K, V}) -> os:putenv(K, V) end, SavedEnv),
        file:del_dir_r(Dir)
    end.

%% Persist in an isolated config directory and verify the standard loader can
%% restore the preference, without writing the developer's real configuration.
machine_preference_test_() ->
    {timeout, 30, fun machine_preference_fixture/0}.

machine_preference_fixture() ->
    Dir = "/tmp/zotonic-editor-config-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Env = os:getenv("ZOTONIC_CONFIG_DIR"),
    SavedId = application:get_env(zotonic, development_editor_id),
    SavedEditor = application:get_env(zotonic, development_editor),
    SavedContainer = application:get_env(zotonic, development_editor_browser_only),
    try
        os:putenv("ZOTONIC_CONFIG_DIR", Dir),
        application:set_env(zotonic, development_editor, {"/usr/bin/true", []}),
        case z_development_editor:browser_only() of
            true -> ok;
            false ->
                ok = z_development_editor:save_preference(<<"configured">>, ?FILE),
                Config = filename:join([Dir, "config.d", "zotonic-editor.config"]),
                ?assertEqual({ok, [[{zotonic, [{development_editor_id, <<"configured">>}]}]]},
                    file:consult(Config)),
                ?assert(lists:member(Config, z_config_files:zotonic_config_files())),
                ?assertEqual(<<"configured">>, z_config:get(development_editor_id)),
                application:set_env(zotonic, development_editor_id, <<"missing-editor">>),
                #{selected := Selected} = z_development_editor:detect(?FILE),
                ?assertEqual(false, maps:get(available, Selected)),
                ?assertEqual({error, choose_editor},
                    z_development_editor:save_preference(<<"missing-editor">>, ?FILE)),
                ok = z_development_editor:save_preference(<<"auto">>, ?FILE),
                ?assertEqual(<<"auto">>, z_config:get(development_editor_id))
        end,
        application:set_env(zotonic, development_editor_browser_only, true),
        ?assertEqual({error, browser_only}, z_development_editor:save_preference(<<"auto">>, ?FILE))
    after
        case Env of false -> os:unsetenv("ZOTONIC_CONFIG_DIR"); _ -> os:putenv("ZOTONIC_CONFIG_DIR", Env) end,
        restore(development_editor_id, SavedId),
        restore(development_editor, SavedEditor),
        restore(development_editor_browser_only, SavedContainer),
        file:del_dir_r(Dir)
    end.

source_location_test() ->
    {ok, File, Line} = z_development_editor:source_location(?MODULE, source_location_test),
    {ok, Source} = file:read_file(File),
    ?assertEqual(<<"source_location_test() ->">>,
        lists:nth(Line, binary:split(Source, <<"\n">>, [global]))),
    ?assertEqual({error, function_not_found},
        z_development_editor:source_location(?MODULE, absent_function)).
