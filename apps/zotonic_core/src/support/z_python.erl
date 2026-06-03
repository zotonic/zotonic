%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Helpers for managing Python virtual environments.
%%
%% The functions in this module are generic helpers for modules that need a
%% private Python environment. A module can pass either a concrete virtualenv
%% directory or the module's application atom. When an application atom is
%% supplied, the virtualenv directory is resolved as:
%%
%% <pre>
%% filename:join(AppDataDir, "venv")
%% </pre>
%%
%% where `AppDataDir` is returned by `z_config_files:app_data_dir/1`.
%%
%% For example, a module can use:
%%
%% <pre>
%% ok = z_python:ensure_venv(zotonic_mod_example),
%% ok = z_python:pip_install(
%%     z_python:venv_python(zotonic_mod_example),
%%     Requirements)
%% </pre>
%%
%% Keeping virtual environments in the module-specific app data directory makes
%% them persistent across restarts and shareable by all sites using the same
%% Zotonic node, while keeping module-specific files out of `zotonic_core`.
%% The default Python executable is read from `z_config:get(python_command)`,
%% which defaults to `python3`.
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

-module(z_python).

-export([
    ensure_venv/1,
    ensure_venv/2,
    pip_install/2,
    python_command/0,
    python_command/1,
    run_install_command/1,
    venv_python/1
]).

-define(KILL_TIMEOUT_SECS, 10).

-spec ensure_venv(Venv) -> ok | {error, Reason} when
    Venv :: file:filename_all() | atom(),
    Reason :: term().
%% @doc Create a Python virtual environment with the default Python command.
ensure_venv(Venv) ->
    ensure_venv(python_command(), Venv).

-spec ensure_venv(Python, Venv) -> ok | {error, Reason} when
    Python :: file:filename_all(),
    Venv :: file:filename_all() | atom(),
    Reason :: term().
%% @doc Create a Python virtual environment if it does not exist.
ensure_venv(Python, Venv) ->
    VenvDir = venv_dir(Venv),
    case filelib:is_file(venv_python(VenvDir)) of
        true ->
            ok;
        false ->
            filelib:ensure_dir(filename:join(VenvDir, ".keep")),
            run_install_command([ Python, "-m", "venv", VenvDir ])
    end.

-spec python_command() -> binary().
%% @doc Return the default Python command, resolved to an absolute path if possible.
python_command() ->
    python_command(z_config:get(python_command)).

-spec python_command(Command) -> binary() when
    Command :: unicode:chardata().
%% @doc Resolve a Python command to an absolute executable path when possible.
python_command(Command) ->
    resolve_command(z_string:trim(z_convert:to_binary(Command))).

-spec pip_install(VenvPython, Requirements) -> ok | {error, Reason} when
    VenvPython :: file:filename_all(),
    Requirements :: file:filename_all(),
    Reason :: term().
%% @doc Upgrade pip and install packages from a requirements file.
pip_install(VenvPython, Requirements) ->
    case run_install_command([ VenvPython, "-m", "pip", "install", "--upgrade", "pip" ]) of
        ok ->
            run_install_command([ VenvPython, "-m", "pip", "install", "--requirement", Requirements ]);
        {error, _} = Error ->
            Error
    end.

-spec run_install_command(Command) -> ok | {error, map()} when
    Command :: [ file:filename_all() ].
%% @doc Run a Python installation command and normalize its result.
run_install_command(Command) ->
    case exec:run(Command, [sync, stdout, stderr, {kill_timeout, ?KILL_TIMEOUT_SECS}]) of
        {ok, _Output} ->
            ok;
        {error, Reason} ->
            {error, #{ command => Command, reason => Reason }}
    end.

-spec venv_python(Venv) -> file:filename_all() when
    Venv :: file:filename_all() | atom().
%% @doc Return the Python executable path inside a virtual environment.
venv_python(Venv) ->
    VenvDir = venv_dir(Venv),
    filename:join([ VenvDir, "bin", "python" ]).

-spec venv_dir(Venv) -> file:filename_all() when
    Venv :: file:filename_all() | atom().
%% @doc Resolve a virtualenv directory or module atom to a concrete directory.
venv_dir(Module) when is_atom(Module) ->
    {ok, Dir} = z_config_files:app_data_dir(Module),
    filename:join(Dir, "venv");
venv_dir(VenvDir) ->
    VenvDir.

%% @doc Resolve a configured Python command to an absolute executable when possible.
resolve_command(<<>>) ->
    <<>>;
resolve_command(Command) ->
    CommandList = unicode:characters_to_list(Command),
    case filename:pathtype(CommandList) of
        relative ->
            resolve_relative_command(CommandList, Command);
        _ ->
            Command
    end.

%% @doc Resolve a bare relative executable name using the OS path.
resolve_relative_command(CommandList, Command) ->
    case lists:member($/, CommandList) of
        true ->
            Command;
        false ->
            case os:find_executable(CommandList) of
                false -> Command;
                Path -> z_convert:to_binary(Path)
            end
    end.
