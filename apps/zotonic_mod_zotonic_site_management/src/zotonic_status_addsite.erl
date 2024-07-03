%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Add a new site

%% Copyright 2016 Marc Worrell
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

-module(zotonic_status_addsite).

-export([
    addsite/3,
    site_ebin_dir/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec addsite(binary(), list(), z:context()) ->
    {ok, {Site :: atom(), Options :: list()}} | {error, term()}.
addsite(Name, Options, Context) when is_binary(Name) ->
    % Check if name can used for the site (not z_, zotonic_, existing site, or existing module)
    case check_name(Name, Context) of
        ok ->
            case filelib:is_file(site_root_dir(Name)) of
                true ->
                    {error, iolist_to_binary([
                        ?__(<<"There is already a file or directory named">>, Context),
                        " ",
                        site_root_dir(Name)])};
                false ->
                    addsite_check_hostname(Name, Options, Context)
            end;
        {error, _} = Error ->
            Error
    end.

% Check Hostname (must have DNS resolve)
addsite_check_hostname(Name, Options, Context) ->
    mod_zotonic_site_management:progress(Name, ?__(<<"Resolving the hostname ...">>, Context), Context),
    {hostname, HostPort} = proplists:lookup(hostname, Options),
    [ Host | _ ] = binary:split( z_convert:to_binary(HostPort), <<":">> ),
    case inet:gethostbyname( z_convert:to_list(Host) ) of
        {ok, _} ->
            addsite_check_db(Name, Options, Context);
        {error, nxdomain} ->
            Error = iolist_to_binary([
                ?__(<<"The hostname is unknown, check your DNS or /etc/hosts file">>, Context),
                ": ", Host
            ]),
            {error, Error}
    end.

% Check if we can connect to the database

-spec addsite_check_db(binary(), list(), z:context()) ->
    {ok, {atom(), list()}} | {error, term()}.
addsite_check_db(Name, Options, Context) ->
    case z_convert:to_binary( proplists:get_value(skeleton, Options) ) of
        <<"nodb">> ->
            Options1 = [
                {dbdatabase, none}
                | proplists:delete(dbdatabase, Options)
            ],
            addsite_check_userdir(Name, Options1, Context);
        _ ->
            mod_zotonic_site_management:progress(Name, ?__(<<"Checking database ...">>, Context), Context),
            DbDatabase = z_convert:to_list(get_fallback(dbdatabase, Options, z_config:get(dbdatabase))),
            ConnectOptions = [
                {dbhost, z_convert:to_list(get_fallback(dbhost, Options, z_config:get(dbhost)))},
                {dbport, z_convert:to_integer(get_fallback(dbport, Options, z_config:get(dbport)))},
                {dbuser, z_convert:to_list(get_fallback(dbuser, Options, z_config:get(dbuser)))},
                {dbpassword, z_convert:to_list(get_fallback(dbpassword, Options, z_config:get(dbpassword)))},
                {dbdatabase, DbDatabase},
                {dbschema, z_convert:to_list(get_fallback(dbschema, Options, Name))}
            ],
            case z_db:ensure_database(DbDatabase, ConnectOptions) of
                ok ->
                    Site = binary_to_atom(Name, utf8),
                    case z_db:ensure_schema(Site, ConnectOptions) of
                        ok ->
                            addsite_check_userdir(Name, Options, Context);
                        {error, _} ->
                            {error, ?__(<<"Could not create the schema in the database.">>, Context)}
                    end;
                {error, _} ->
                    {error, ?__(<<"Could not connect to 'postgres' database or create the database.">>, Context)}
            end
    end.

% Check if the user directory is writeable
-spec addsite_check_userdir(binary(), list(), z:context()) ->
    {ok, {atom(), list()}} | {error, term()}.
addsite_check_userdir(Name, Options, Context) ->
    SiteDir = site_root_dir(Name),
    case file:make_dir(SiteDir) of
        ok ->
            addsite_check_git(Name, Options, Context);
        {error, Error} ->
            report_error(Error, SiteDir, Context)
    end.

% If Git: checkout from Git
-spec addsite_check_git(binary(), list(), z:context()) ->
    {ok, {atom(), list()}} | {error, term()}.
addsite_check_git(Name, Options, Context) ->
    case z_string:trim( z_convert:to_binary( proplists:get_value(git, Options, <<>>) ) ) of
        <<>> ->
            SiteDir = site_root_dir(Name),
            Cmd = lists:flatten([
                "git init -q ",
                z_filelib:os_filename(SiteDir)
                ]),
            _ = os:cmd(Cmd),
            create_gitignore(SiteDir),
            addsite_check_skel(Name, Options, Context);
        Git ->
            case file:del_dir(site_root_dir(Name)) of
                ok ->
                    mod_zotonic_site_management:progress(Name, ?__(<<"Git checkout ...">>, Context), Context),
                    Cmd = lists:flatten([
                        "git clone -q --recurse-submodules ",
                        z_filelib:os_filename(Git),
                        " ",
                        z_filelib:os_filename(site_root_dir(Name))
                        ]),
                    case os:cmd(Cmd) of
                        [] ->
                            addsite_check_skel(Name, Options, Context);
                        Error ->
                            ?LOG_ERROR(#{
                                text => <<"[zotonic_site_status] Could not checkout Git repository">>,
                                in => zotonic_mod_zotonic_site_management,
                                site => Name,
                                git_url => Git,
                                site_dir => site_root_dir(Name),
                                result => error,
                                reason => Error
                            }),
                            {error, [   ?__(<<"Could not check out Git repository:">>, Context),
                                        " ", Error]}
                    end;
                {error, Error} ->
                    {error, [   ?__(<<"Could not delete the site dir for the Git checkout:">>, Context),
                                " ", z_convert:to_binary(Error)]}
            end
    end.

% Make directory, copy 'priv/skel/<skel> skeleton, replace config.in and SITE.erl on the fly
% Do not copy files that would overwrite any file or directory from Git
addsite_check_skel(Name, Options, Context) ->
    case ensure_dirs(site_dirs(Name, Options), Context) of
        ok ->
            addsite_copy_skel(Name, Options, Context);
        {error, _} = Error ->
            Error
    end.

addsite_copy_skel(Name, Options, Context) ->
    mod_zotonic_site_management:progress(Name, ?__(<<"Copy skeleton files ...">>, Context), Context),
    SiteDir = guess_site_dir(Name, Options),
    case skel_dir(Options) of
        undefined ->
            {error, <<"No site skeleton selected">>};
        SkelDir ->
            case filelib:is_dir(SkelDir) of
                false ->
                    {error, <<"Site skeleton directory could not be found">>};
                true ->
                    Options1 = [
                        {site, Name},
                        {sign_key, z_ids:id(20)},
                        {sign_key_simple, z_ids:id(12)}
                        | Options
                    ],
                    Options2 = case z_utils:is_empty(proplists:get_value(admin_password, Options1)) of
                        true  ->
                            [ {admin_password, z_ids:id(12)} | Options1 ];
                        false ->
                            Options1
                    end,
                    case copy_skeleton_dir(SkelDir, SiteDir, Options2, Context) of
                        ok ->
                            addsite_compile(Name, Options2, Context);
                        {error, _} = Error ->
                            Error
                    end
            end
    end.

% Compile
-spec addsite_compile(binary(), list(), z:context()) -> {ok, {atom(), list()}} | {error, term()}.
addsite_compile(Name, Options, Context) ->
    Site = binary_to_atom(Name, utf8),
    case erlang:get(is_zotonic_command) of
        true ->
            {ok, {Site, Options}};
        _ ->
            mod_zotonic_site_management:progress(
                Name,
                ?__(<<"Force compile all Erlang files ...">>, Context),
                Context
            ),
            zotonic_filehandler:compile_all_sync(),
            EbinPath = site_ebin_dir(Name),
            case code:add_path(EbinPath) of
                true ->
                    case application:load(Site) of
                        ok ->
                            {ok, {Site, Options}};
                        {error, {already_loaded, Site}} ->
                            {ok, {Site, Options}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

% Add a sample .gitgnore file to the newly created site directory.
create_gitignore(SiteDir) ->
    GitIgnore = <<
        "files\n"
        "zotonic_site.config\n"
        ".eunit\n"
        ".rebar3\n"
        "*.beam\n",
        "*.o\n",
        "*.plt\n"
        "*~\n"
        "*#\n"
    >>,
    file:write_file(filename:join([SiteDir, ".gitignore"]), GitIgnore).


-spec copy_skeleton_dir(file:filename_all(), file:filename_all(), list(), z:context()) -> ok | {error, Reason :: binary() | atom()}.
copy_skeleton_dir(From, To, Options, Context) ->
    Files = filelib:wildcard(z_convert:to_list(filename:join(From, "*"))),
    lists:foldl(
            fun
                (FromPath, ok) ->
                    case filename:basename(FromPath) of
                        "." -> ok;
                        ".." -> ok;
                        ".git" -> ok;
                        Basename ->
                            ToPath = filename:join([To, Basename]),
                            case filelib:is_dir(FromPath) of
                                true ->
                                    case filelib:is_file(ToPath) of
                                        false ->
                                            case file:make_dir(ToPath) of
                                                ok ->
                                                    copy_skeleton_dir(FromPath, ToPath, Options, Context);
                                                {error, Reason} ->
                                                    ?LOG_ERROR(#{
                                                        text => <<"[zotonic_site_status] Error creating directory">>,
                                                        in => zotonic_mod_zotonic_site_management,
                                                        directory => ToPath,
                                                        result => error,
                                                        reason => Reason
                                                    }),
                                                    {error, iolist_to_binary([
                                                            ?__(<<"Could not create the directory">>, Context),
                                                            " ",
                                                            ToPath
                                                        ])}
                                            end;
                                        true ->
                                            % Stop recursion, directory exists in both places
                                            ok
                                    end;
                                false ->
                                    copy_file(Basename, FromPath, ToPath, Options)
                            end
                    end;
                (_FromPath, {error, _} = Error) ->
                    Error
            end,
            ok,
            Files).

copy_file("SITE" ++ Ext = Filename, FromPath, ToPath, Options)
    when Ext == "_sup.erl"; Ext == "_app.erl" ->
    case proplists:get_value(app, Options, false) of
        true ->
            copy_site_file(Filename, FromPath, ToPath, Options);
        false ->
            ok
    end;
copy_file("SITE" ++ Ext = Filename, FromPath, ToPath, Options)
    when Ext == ".erl"; Ext == ".app.src"; Ext == "_sup.erl"; Ext == "_app.erl" ->
    copy_site_file(Filename, FromPath, ToPath, Options);
copy_file("zotonic_site.config.in", FromPath, ToPath, Options) ->
    % First replace the tags in the config.in, then copy it.
    {ok, Bin} = file:read_file(FromPath),
    Outfile = replace_tags(Bin, Options),
    ok = file:write_file(ToPath, Outfile),

    % Merge the optionally pre-existing config and config.in to a rewritten config.
    FnConfig = filename:join([filename:dirname(ToPath), "zotonic_site.config"]),
    Cfg = case filelib:is_file(FnConfig) of
        true ->
            % Merge config files
            {ok, [Config]} = file:consult(FnConfig),
            {ok, [ConfigIn]} = file:consult(ToPath),
            MergedConfigs = lists:ukeymerge(1, lists:sort(Config), lists:sort(ConfigIn)),
            io_lib:format("~p.", [normalize_options(MergedConfigs)]);
        false ->
            Outfile
    end,
    case file:write_file(FnConfig, Cfg) of
        ok ->
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"[zotonic_site_status] Error writing site config file">>,
                in => zotonic_mod_zotonic_site_management,
                result => error,
                reason => Reason,
                filename => FnConfig
            }),
            Error
    end;
copy_file(_Filename, FromPath, ToPath, _Options) ->
    case filelib:is_file(ToPath) of
        true -> ok;
        false ->
            case file:copy(FromPath, ToPath)  of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end
    end.

copy_site_file("SITE" ++ Ext, FromPath, ToPath, Options) ->
    % Replace with the site name
    ToPath1 = filename:join([
                    filename:dirname(ToPath),
                    z_convert:to_list(proplists:get_value(site, Options)) ++ Ext
                ]),
    case filelib:is_file(ToPath1) of
        true ->
            ok;
        false ->
            % Replace the module name, write the site Erlang module.
            {ok, SiteErl} = file:read_file(FromPath),
            Outfile = replace_tags(SiteErl, Options),
            file:write_file(ToPath1, Outfile)
    end.

normalize_options(Options) ->
    lists:flatten(
        lists:map(
            fun
                ({dbdatabase, "none"}) ->
                    {dbdatabase, none};
                ({skeleton, Skel}) ->
                    {skeleton, z_convert:to_atom(Skel)};
                ({K,V}) when is_binary(V) ->
                    {K, binary_to_list(V)};
                (KV) ->
                    KV
            end,
            Options)).

site_ebin_dir(Name) ->
    ZotonicApps = z_convert:to_list( z_path:zotonic_apps() ),
    Dir = case filename:basename(ZotonicApps) of
        "_checkouts" ->
            filename:join([ ZotonicApps, Name, "ebin" ]);
        _ ->
            filename:join([ z_path:build_lib_dir(), Name, "ebin" ])
    end,
    z_convert:to_list(Dir).

site_dirs(Name, Options) ->
    Tokens = site_dir_tokens(Name, Options),
    BaseDir = z_path:zotonic_apps(),
    do_site_dirs(Tokens, BaseDir, []).

do_site_dirs([Token | Tokens], Prev, Acc) ->
    Dir = filename:join(Prev, Token),
    do_site_dirs(Tokens, Dir, [Dir | Acc]);
do_site_dirs([], _Prev, Acc) ->
    lists:reverse(Acc).

site_dir_tokens(Name, Options) ->
    case proplists:get_value(umbrella, Options, false) of
        true ->
            site_umbrella_dir_tokens(Name);
        false ->
            site_root_dir_tokens(Name)
    end.

guess_site_dir(Name, Options) ->
    case proplists:get_value(umbrella, Options, false) of
        true ->
            site_umbrella_dir(Name);
        false ->
            site_root_dir(Name)
    end.

site_root_dir_tokens(Name) ->
    [Name].

site_umbrella_dir_tokens(Name) ->
    [Name, <<"apps">>, Name].

site_root_dir(Name) ->
    do_site_dir(site_root_dir_tokens(Name)).

site_umbrella_dir(Name) ->
    do_site_dir(site_umbrella_dir_tokens(Name)).

do_site_dir(Tokens) ->
    z_convert:to_list(filename:join([ z_path:zotonic_apps() | Tokens ])).

skel_dir(Options) ->
    case z_string:to_name(proplists:get_value(skeleton, Options, <<"empty">>)) of
        <<>> -> undefined;
        <<"_">> -> undefined;
        Skel ->
            PrivDir = code:priv_dir(zotonic_mod_zotonic_site_management),
            filename:join([ PrivDir, "skel", Skel ])
    end.

ensure_dirs([Dir | Dirs], Context) ->
    case ensure_dir(Dir, Context) of
        ok ->
            ensure_dirs(Dirs, Context);
        {error, _} = Error ->
            Error
    end;
ensure_dirs([], _Context) ->
    ok.

ensure_dir(Dir, Context) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, eacces} ->
            {error, iolist_to_binary([
                    ?__("Not allowed to create the site directory", Context),
                    ": ",
                    Dir])};
        {error, _} ->
            {error, iolist_to_binary([
                    ?__("Could not create the site directory", Context),
                    ": ",
                    Dir])}
    end.

-spec replace_tags(binary(), list()) -> list(binary()).
replace_tags(Bin, Options) when is_binary(Bin) ->
    Parts = re:split(Bin, "(%%[A-Z]+%%)", [{return,binary}]),
    [map_tag(P, Options) || P <- Parts].

-spec map_tag(term(), list()) -> term().
map_tag(<<"%%SITE%%">>, Options) -> proplists:get_value(site, Options);
map_tag(<<"%%SITEHOSTNAME%%">>, Options) -> proplists:get_value(hostname, Options);
map_tag(<<"%%SKEL%%">>, Options) ->
    case proplists:get_value(skeleton, Options, <<>>) of
        <<>> -> "undefined";
        Skel -> Skel
    end;
map_tag(<<"%%FULLNAME%%">>, _Options) -> <<>>;
map_tag(<<"%%DBHOST%%">>, Options) -> proplists:get_value(dbhost, Options, "");
map_tag(<<"%%DBPORT%%">>, Options) ->
    case proplists:get_value(dbport, Options, <<>>) of
        <<>> -> "0";
        Port -> Port
    end;
map_tag(<<"%%DBUSER%%">>, Options) -> proplists:get_value(dbuser, Options, "");
map_tag(<<"%%DBPASSWORD%%">>, Options) -> proplists:get_value(dbpassword, Options, "");
map_tag(<<"%%DBDATABASE%%">>, Options) -> proplists:get_value(dbdatabase, Options, "");
map_tag(<<"%%DBSCHEMA%%">>, Options) -> proplists:get_value(dbschema, Options, "");
map_tag(<<"%%ADMINPASSWORD%%">>, Options) -> proplists:get_value(admin_password, Options);
map_tag(<<"%%SIGNKEY%%">>, Options) -> proplists:get_value(sign_key, Options);
map_tag(<<"%%SIGNKEYSIMPLE%%">>, Options) -> proplists:get_value(sign_key_simple, Options);
map_tag(<<"%%YEAR%%">>, _Options) ->  z_dateformat:format(calendar:local_time(), "Y", []);
map_tag(<<"%%DATE%%">>, _Options) -> z_dateformat:format(calendar:local_time(), "Y-m-d", []);
map_tag(<<"%%APPSRCMOD%%">>, Options) ->
    case proplists:get_value(app, Options, false) of
        true ->
            Site = proplists:get_value(site, Options),
            <<"{", Site/binary, "_app, []}">>;
        false ->
            <<"[]">>
    end;
map_tag(Bin, _Options) -> Bin.


get_fallback(Opt, Options, Default) ->
    case proplists:get_value(Opt, Options, Default) of
        <<>> -> Default;
        undefined -> Default;
        Val -> Val
    end.

check_name(Name, Context) ->
    case is_reserved(Name) of
        true ->
            {error, ?__(<<"This name is reserved">>, Context)};
        false ->
            Name1 = binary_to_atom(Name, utf8),
            case is_module(Name1) of
                true -> {error, ?__(<<"This name is taken by another Erlang module">>, Context)};
                false -> ok
            end
    end.

is_module(Module) ->
    try
        {ok, _} = z_utils:ensure_existing_module(Module),
        true
    catch
        _:_ -> false
    end.

is_reserved(<<"erlang">>) -> true;
is_reserved(<<"z_", _/binary>>) -> true;
is_reserved(<<"zotonic_", _/binary>>) -> true;
is_reserved(<<"mod_", _/binary>>) -> true;
is_reserved(<<"scomp_", _/binary>>) -> true;
is_reserved(<<"filter_", _/binary>>) -> true;
is_reserved(_) -> false.


report_error(eacces, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__(<<"No permission to create the site directory at:">>, Context),
                " ", SiteDir
            ])};
report_error(eexist, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__(<<"The file or directory already exists:">>, Context),
                " ", SiteDir
            ])};
report_error(enoent, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__(<<"The parent directory does not exist:">>, Context),
                " ", SiteDir
            ])};
report_error(enospc, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__("Disk full creating:", Context),
                " ", SiteDir
            ])};
report_error(enotdir, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__(<<"A component of the path is not a directory:">>, Context),
                " ", SiteDir
            ])};
report_error(Error, SiteDir, Context) ->
    {error, iolist_to_binary([
                ?__(<<"Could not create the file:">>, Context),
                " ", SiteDir, " (", z_convert:to_binary(Error), ")"
            ])}.
