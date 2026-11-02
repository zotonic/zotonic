%% @copyright 2026 Marc Worrell
%% @doc Deploy a git working tree without activating a partially built schema.
-module(zotonic_cmd_deploy).
-export([info/0, run/1]).

info() ->
    "Restore rebar.lock, pull (--ff-only), compile and activate under the schema-upgrade barrier.".

run([]) ->
    run([zotonic_command:get_zotonic_dir()]);
run([Path]) ->
    case zotonic_command:net_start() of
        ok ->
            {ok, Target} = zotonic_command:get_target_node(),
            %% Building can take much longer than the default RPC timeout.
            Result = rpc:call(Target, zotonic_filehandler_compile, deploy,
                [git, filename:absname(Path)], infinity),
            case Result of
                {ok, Output} -> io:put_chars(Output);
                Error ->
                    io:format(standard_error, "Deployment failed: ~p~n", [Error]),
                    halt(1)
            end;
        Error -> zotonic_command:format_error(Error)
    end;
run(_) ->
    io:format(standard_error, "Usage: zotonic deploy [git-working-tree]~n", []),
    halt(1).
