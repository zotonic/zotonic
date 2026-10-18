%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Start or probe the stdio language server through Zotonic's authenticated node connection.
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

-module(zotonic_cmd_lsp).
-export([info/0, run/1]).

%% @doc Describe the editor transport in the CLI command list.
info() -> "Run the template language server over stdio (requires a running Zotonic node).".

%% @doc Connect using the normal local Erlang cookie; do not expose a network service.
run(["--probe"]) ->
    _ = logger:set_handler_config(default, config, #{type => standard_error}),
    case zotonic_command:net_start("lsp_probe" ++ os:getpid()) of
        ok ->
            {ok, Node} = zotonic_command:get_target_node(),
            case rpc:call(Node, z_development_lsp_backend, sites, [], 2000) of
                Sites when is_list(Sites) -> halt(0);
                _ -> halt(1)
            end;
        _ -> halt(1)
    end;
run([]) ->
    _ = logger:set_handler_config(default, config, #{type => standard_error}),
    case zotonic_command:net_start("lsp" ++ os:getpid()) of
        ok ->
            {ok, Node} = zotonic_command:get_target_node(),
            z_development_lsp:run(Node);
        {error, _} = Error -> zotonic_command:format_error(Error)
    end;
run(_) ->
    io:format(standard_error, "USAGE: zotonic lsp [--probe]~n", []),
    halt(1).
