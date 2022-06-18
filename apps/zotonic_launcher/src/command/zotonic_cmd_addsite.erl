%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Addsite CLI command

%% Copyright 2019 Marc Worrell
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

-module(zotonic_cmd_addsite).
-author("Marc Worrell").

-behaviour(zotonic_command).

%% API
-export([
    info/0,
    usage/0,
    run/1
]).

-include("../../include/zotonic_command.hrl").

-define(SKEL, "blog").
-define(APP, false).
-define(UMBRELLA, false).

cmd_info() ->
    #cmd_info{
        name        = <<"addsite">>,
        description = <<"Add a site and install database">>,
        run         = <<"zotonic addsite [options] <site_name>">>,
        options = [
            #cmd_option{
                name        = <<"skeleton">>,
                description = <<"Skeleton site">>,
                arg         = <<"s">>,
                values      = [<<"blog">>, <<"empty">>, <<"nodb">>],
                default     = <<"blog">>
            },
            #cmd_option{
                name        = <<"hostname">>,
                description = <<"Site's hostname">>,
                arg         = <<"H">>,
                default     = <<"<site_name>.test">>
            },
            #cmd_option{
                name        = <<"site_dir">>,
                description = <<"Create the site in the current directory and add a symlink to zotonic app_user">>,
                arg         = <<"L">>,
                default     = <<"<cwd>">>
            },
            #cmd_option{
                name        = <<"git">>,
                description = <<"Clone from this Git url, before copying skeleton">>,
                arg         = <<"G">>
            },
            #cmd_option{
                name        = <<"dbhost">>,
                description = <<"Database host">>,
                arg         = <<"h">>,
                default     = <<"localhost">>
            },
            #cmd_option{
                name        = <<"dbport">>,
                description = <<"Database port">>,
                arg         = <<"p">>,
                default     = <<"5432">>
            },
            #cmd_option{
                name        = <<"dbuser">>,
                description = <<"Database user">>,
                arg         = <<"u">>,
                default     = <<"zotonic">>
            },
            #cmd_option{
                name        = <<"dbpassword">>,
                description = <<"Database password">>,
                arg         = <<"p">>,
                default     = <<"zotonic">>
            },
            #cmd_option{
                name        = <<"dbdatabase">>,
                description = <<"Database name">>,
                arg         = <<"d">>,
                default     = <<"zotonic">>
            },
            #cmd_option{
                name        = <<"dbschema">>,
                description = <<"Database schema">>,
                arg         = <<"n">>,
                default     = <<"public">>
            },
            #cmd_option{
                name        = <<"admin_password">>,
                description = <<"Admin password">>,
                arg         = <<"a">>,
                default     = <<"<random>">>
            },
            #cmd_option{
                name        = <<"app">>,
                description = <<"If true, initializes a site app and a root supervisor when the site starts">>,
                arg         = <<"A">>,
                default     = <<"false">>
            },
            #cmd_option{
                name        = <<"umbrella">>,
                description = <<"If true, the site dir becomes a multi-app structure">>,
                arg         = <<"U">>,
                default     = <<"false">>
            }
        ]
    }.

info() ->
    Info = cmd_info(),
    Info#cmd_info.description.

usage() ->
    io:format("~n~n/* Auto generated~n *-----------------*/~n~n"),

    Info = #cmd_info{options = Options} = cmd_info(),
    Title = do_usage_title(Info),
    io:format("~s~n~n", [Title]),
    LongestOptionNameSize = lists:foldl(
        fun(#cmd_option{name = OptionName}, Size) ->
            NameSize = size(OptionName),
            case NameSize > Size of
                true ->
                    NameSize;
                false ->
                    Size
            end
        end,
        0,
        Options
    ),
    BeginIndent = 1,
    Prefix = binary:copy(<<" ">>, BeginIndent),
    ExtraNPad = 3,
    Pad = LongestOptionNameSize + ExtraNPad,
    lists:foreach(
        fun(Option) ->
            OptionInfo = do_option_info(Option, Pad),
            io:format("~s~s~n", [Prefix, OptionInfo])
        end,
        Options
    ),

    io:format("~n~n/* Old one~n *-----------------*/~n~n"),

    io:format("Usage: zotonic addsite [options] <site_name> ~n~n"),
    io:format(" -s <skel>        Skeleton site (one of 'blog', 'empty', 'nodb'; default: ~s~n", [?SKEL]),
    io:format(" -H <host>        Site's hostname (default: <site_name.test>) ~n"),
    io:format(" -L               Create the site in the current directory and add a symlink to zotonic app_user~n"),
    io:format(" -G <url>         Clone from this Git url, before copying skeleton~n"),
    io:format(" -h <host>        Database host (default: ~s) ~n", [ z_config:get(dbhost) ]),
    io:format(" -p <port>        Database port (default: ~p) ~n", [ z_config:get(dbport) ]),
    io:format(" -u <user>        Database user (default: ~s) ~n", [ z_config:get(dbuser) ]),
    io:format(" -P <pass>        Database password (default: ~s) ~n", [ z_config:get(dbpassword) ]),
    io:format(" -d <name>        Database name (default: ~s) ~n", [ z_config:get(dbdatabase) ]),
    io:format(" -n <schema>      Database schema (defaults to <site_name>) ~n"),
    io:format(" -a <pass>        Admin password~n"),
    io:format(" -A <app>         If true, initializes a site app and a root supervisor when the site starts (default: ~p)~n", [?APP]),
    io:format(" -U <umbrella>    If true, the site dir becomes a multi-app structure (default: ~p)~n~n", [?UMBRELLA]).

do_usage_title(#cmd_info{run = Run}) ->
    <<"Usage: ", Run/binary>>.

do_option_info(
    #cmd_option{
        name = Name,
        description = Description,
        arg = Arg,
        values = Values,
        default = Default
    },
    NPad
) ->
    Pad = binary:copy(<<" ">>, NPad - size(Name)),
    Info1 = <<"-", Arg/binary, " <", Name/binary, "> ", Pad/binary, Description/binary>>,
    Info2 = case Values =:= [] of
        true ->
            Info1;
        false ->
            ValuesSufix = case Default =:= <<>> of
                true ->
                    <<")">>;
                false ->
                    <<";">>
            end,
            ValuesInfo = do_option_values_info(Values),
            <<Info1/binary, " (one of ", ValuesInfo/binary, ValuesSufix/binary>>
    end,
    case Default =:= <<>> of
        true ->
            Info2;
        false ->
            DefaultSufix = case Values =:= [] of
                true ->
                    <<" (">>;
                false ->
                    <<" ">>
            end,
            <<Info2/binary, DefaultSufix/binary, "default: ", Default/binary, ")">>
    end.

do_option_values_info(Values) ->
    do_option_values_info(Values, <<>>).

do_option_values_info([], Acc) ->
    Acc;
do_option_values_info([Value], Acc) ->
    <<Acc/binary, "'", Value/binary, "'">>;
do_option_values_info([Value | Values], Acc) ->
    AccOut = <<Acc/binary, "'", Value/binary, "', ">>,
    do_option_values_info(Values, AccOut).

run(Args) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            run_target(Target, Args);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

run_target(Target, Args) ->
    ZotonicConfigFiles = zotonic_launcher_config:zotonic_config_files(Target),
    case zotonic_launcher_config:read_configs(ZotonicConfigFiles) of
        {ok, Cfg} ->
            zotonic_launcher_config:load_configs(Cfg),
            run_parse_args(Target, Args);
        {error, _} = ZError ->
            zotonic_command:format_error(ZError)
    end.

run_parse_args(_Target, []) ->
    usage();
run_parse_args(Target, Args) ->
    case parse(Args) of
        {error, Arg} ->
            io:format(standard_error, "Unknown argument ~p~n~n", [ Arg ]),
            usage(),
            halt(1);
        {ok, {Options, [ Sitename ]}} ->
            case is_valid_sitename(Sitename) of
                true ->
                    Options1 = maybe_default_hostname(Sitename, Options),
                    addsite(Target, Sitename, Options1);
                false ->
                    io:format(standard_error,
                              "Invalid site name \"~s\", only use 'a-z', followed by 'a-z', 'A-Z', '0-9' and '_' characters.~n"
                              "The site name should also not start with 'zotonic_' or 'z_'~n",
                              [ Sitename ]),
                    halt(1)
            end;
        {ok, {_Options, []}} ->
            io:format(standard_error, "Missing site name.~n~n", []),
            usage(),
            halt(1);
        {ok, {_Options, _}} ->
            io:format(standard_error, "More than one site name.~n~n", []),
            usage(),
            halt(1)
    end.

is_valid_sitename("zotonic_" ++ _) ->
    false;
is_valid_sitename("z_" ++ _) ->
    false;
is_valid_sitename(Sitename) ->
    case re:run(Sitename, "^[a-z][a-zA-Z0-9_]*$") of
        {match, _} -> true;
        nomatch -> false
    end.

maybe_default_hostname(Sitename, #{ hostname := undefined } = Options) ->
    Options#{ hostname => Sitename ++ ".test" };
maybe_default_hostname(_Sitename, Options) ->
    Options.

addsite(_Target, Sitename, #{ hostname := undefined }) ->
    io:format(standard_error, "Please specify the hostname, for example: -H ~s.test~n~n", [ Sitename ]),
    halt(1);
addsite(_Target, Sitename, Options0) ->
    Options = set_dbschema(Sitename, Options0),
    Context = z_context:new(zotonic_site_status),
    case zotonic_status_addsite:addsite( z_convert:to_binary(Sitename), maps:to_list(Options), Context ) of
        {ok, {Site, SiteOpts}} ->
            io:format("Created site ~p:~n - url \"~s\"~n - admin password \"~s\"~n",
                      [ Site,
                        proplists:get_value(hostname, SiteOpts),
                        proplists:get_value(admin_password, SiteOpts)
                      ]),
            case zotonic_command:net_start() of
                ok ->
                    io:format("Compiling...~n", []),
                    _ = zotonic_command:rpc(zotonic_filehandler, compile_all_sync, []),
                    case zotonic_command:rpc(z_sites_manager, upgrade, []) of
                        ok ->
                            Res = zotonic_command:rpc(z, shell_startsite, [ Site ]),
                            io:format("Site started: ~p~n", [ Res ]);
                        {error, _} = Error ->
                            io:format(standard_error, "Error upgrading sites on running Zotonic: ~p.~n~n", [ Error ]),
                            exit(1)
                    end;
                {error, _} ->
                    io:format(standard_error, "Zotonic is not running, site is not compiled and not started.~n", []),
                    exit(1)
            end;
        {error, Reason} when is_binary(Reason) ->
            io:format(standard_error, "Error: ~s~n", [ Reason ]),
            halt(1);
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [ Reason ]),
            halt(1)
    end.

set_dbschema(Sitename, Options) ->
    DefaultDb = z_convert:to_list( z_config:get(dbdatabase) ),
    case maps:get(dbschema, Options, "") of
        "" ->
            case maps:get(dbdatabase, Options, DefaultDb) of
                DefaultDb -> Options#{ dbschema => Sitename };
                "" -> Options#{ dbschema => Sitename };
                _ -> Options
            end;
        _ ->
            Options
    end.

-spec parse( list() ) -> {ok, {map(), list()}} | {error, string()}.
parse(Args) when is_list(Args) ->
    Options = #{
        hostname => undefined,
        skeleton => ?SKEL,
        app => ?APP,
        umbrella => ?UMBRELLA
    },
    parse_args(Args, Options).

parse_args([ "-s", Skel | Args ], Acc) ->
    parse_args(Args, Acc#{ skeleton => Skel });
parse_args([ "-H", Host | Args ], Acc) ->
    parse_args(Args, Acc#{ hostname => Host });
parse_args([ "-L" | Args ], Acc) ->
    {ok, Cwd} = file:get_cwd(),
    parse_args(Args, Acc#{ site_dir => Cwd });
parse_args([ "-G", GitUrl | Args ], Acc) ->
    parse_args(Args, Acc#{ git => GitUrl });
parse_args([ "-h", Host | Args ], Acc) ->
    parse_args(Args, Acc#{ dbhost => Host });
parse_args([ "-p", Port | Args ], Acc) ->
    parse_args(Args, Acc#{ dbport => Port });
parse_args([ "-u", User | Args ], Acc) ->
    parse_args(Args, Acc#{ dbuser => User });
parse_args([ "-P", Pw | Args ], Acc) ->
    parse_args(Args, Acc#{ dbpassword => Pw });
parse_args([ "-d", Database | Args ], Acc) ->
    parse_args(Args, Acc#{ dbdatabase => Database });
parse_args([ "-n", Schema | Args ], Acc) ->
    parse_args(Args, Acc#{ dbschema => Schema });
parse_args([ "-a", Pw | Args ], Acc) ->
    parse_args(Args, Acc#{ admin_password => Pw });
parse_args([ "-A", App | Args ], Acc) ->
    parse_bool_arg(app, App, Args, Acc);
parse_args([ "-U", Umbrella | Args ], Acc) ->
    parse_bool_arg(umbrella, Umbrella, Args, Acc);
parse_args([ "-" ++ _ = Arg | _ ], _Acc) ->
    {error, Arg};
parse_args(Rest, Acc) ->
    {ok, {Acc, Rest}}.

parse_bool_arg(Key, Arg, Args, Acc) ->
    {ok, RE} = re:compile("^(true|false|1|0)$", [unicode, ucp, caseless]),
    case re:run(Arg, RE, [{capture, none}]) of
        match ->
            Bool = z_convert:to_bool(string:to_lower(Arg)),
            parse_args(Args, Acc#{ Key => Bool });
        nomatch ->
            {error, Arg}
    end.
