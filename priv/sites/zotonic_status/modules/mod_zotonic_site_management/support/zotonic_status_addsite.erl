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
    addsite/3
    ]).

-include_lib("zotonic.hrl").

-spec addsite(binary(), list(), #context{}) -> ok | {error, binary()|string()}.
addsite(Name, Options, Context) when is_binary(Name) ->
    % Check if name can used for the site (not z_, zotonic_, existing site, or existing module)
    case check_name(Name, Context) of
        ok -> 
            case filelib:is_file(site_dir(Name)) of
                true ->
                    {error, iolist_to_binary([
                        ?__("There is already a file or directory named", Context),
                        " ",
                        site_dir(Name)])};
                false ->
                    addsite_check_hostname(Name, Options, Context)
            end;
        {error, _} = Error -> Error
    end.

% Check Hostname (must have DNS resolve)
addsite_check_hostname(Name, Options, Context) ->
    {hostname, HostPort} = proplists:lookup(hostname, Options),
    [Host|_] = string:tokens(z_convert:to_list(HostPort), ":"),
    case inet:gethostbyname(Host) of
        {ok, _} ->
            addsite_check_db(Name, Options, Context);
        {error, nxdomain} ->
            {error, ?__("The hostname is unknown, check your DNS or /etc/hosts file.", Context)}
    end.

% Check if we can connect to the database
addsite_check_db(Name, Options, Context) ->
    case proplists:lookup(skeleton, Options) of
        <<"nodb">> ->
            Options = [
                {dbdatabase, none}
                | proplists:delete(dbdatabase, Options)
            ],
            addsite_check_userdir(Name, Options, Context);
        _ ->
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
                            {error, ?__("Could not create the schema in the database.", Context)}
                    end;
                {error, _} ->
                    {error, ?__("Could not connect to 'postgres' database or create the database.", Context)}
            end
    end.

% Check if the user directory is writeable
addsite_check_userdir(Name, Options, Context) ->
    SiteDir = site_dir(Name),
    case file:make_dir(SiteDir) of
        ok ->
            addsite_check_git(Name, Options, Context);
        {error, Error} ->
            error(Error, SiteDir, Context)
    end.

% If Git: checkout from Git
addsite_check_git(Name, Options, Context) ->
    case z_string:trim(proplists:get_value(git, Options, <<>>)) of
        <<>> ->
            SiteDir = site_dir(Name),
            Cmd = lists:flatten([
                "git init -q ",
                z_utils:os_filename(SiteDir)
                ]),
            _ = os:cmd(Cmd),
            create_gitignore(SiteDir),
            addsite_check_skel(Name, Options, Context);
        Git ->
            case file:del_dir(site_dir(Name)) of
                ok ->
                    Cmd = lists:flatten([
                        "git clone -q --recurse-submodules ",
                        z_utils:os_filename(Git),
                        " ",
                        z_utils:os_filename(site_dir(Name))
                        ]),
                    case os:cmd(Cmd) of
                        [] ->
                            addsite_check_skel(Name, Options, Context);
                        Error ->
                            lager:error("[zotonic_status] Could not checkout ~p to ~p: ~p",
                                        [Git, site_dir(Name), Error]),
                            {error, [   ?__("Could not check out Git repository:", Context),
                                        " ", Error]}
                    end;
                {error, Error} ->
                    {error, [   ?__("Could not delete the site dir for the Git checkout:", Context),
                                " ", z_convert:to_binary(Error)]}
            end
    end.

% Make directory, copy 'priv/skel/<skel> skeleton, replace config.in and SITE.erl on the fly
% Do not copy files that would overwrite any file or directory from Git
addsite_check_skel(Name, Options, Context) ->
    case ensure_dir(site_dir(Name), Context) of
        ok ->
            addsite_copy_skel(Name, Options, Context);
        {error, _} = Error ->
            Error
    end.

addsite_copy_skel(Name, Options, Context) ->
    SiteDir = site_dir(Name),
    case skel_dir(Options) of
        undefined ->
            {error, <<"No site skeleton selected">>};
        SkelDir ->
            case filelib:is_dir(SkelDir) of
                false ->
                    {error, <<"Site skeleton directory could not be found">>};
                true ->
                    case copy_skeleton(Name, SkelDir, SiteDir, Options, Context) of
                        ok ->
                            addsite_compile(Name, Options, Context);
                        {error, _} = Error ->
                            Error
                    end
            end
    end.

% Compile
addsite_compile(Name, Options, Context) ->
    z:compile(),
    addsite_startup(Name, Options, Context).

% Start the new site
addsite_startup(Name, Options, _Context) ->
    Site = binary_to_atom(Name, utf8), 
    _ = z_sites_manager:start(Site),
    {ok, Options}.

% Add a sample .gitgnore file to the newly created site directory.
create_gitignore(SiteDir) ->
    GitIgnore = <<
        "files\n"
        "config\n"
        ".eunit\n"
        ".rebar3\n"
        "*.beam\n",
        "*.o\n",
        "*.plt\n"
    >>,
    file:write_file(filename:join([SiteDir, ".gitignore"]), GitIgnore).


copy_skeleton(Name, SkelDir, SiteDir, Options, Context) ->
    Options1 = [
        {site, Name},
        {admin_password, z_ids:id(12)},
        {sign_key, z_ids:id(20)},
        {sign_key_simple, z_ids:id(12)}
        | Options
    ],
    copy_skeleton_dir(SkelDir, SiteDir, Options1, Context).

copy_skeleton_dir(From, To, Options, Context) ->
    Files = filelib:wildcard(z_convert:to_list(filename:join(From,"*"))),
    lists:foreach(
            fun(FromPath) ->
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
                                            {error, _} = Error ->
                                                lager:error("[zotonic_status] Error creating directory ~p: ~p",
                                                            [ToPath, Error]),
                                                {error, iolist_to_binary([
                                                        ?__("Could not create the directory", Context),
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
                end
            end,
            Files).

copy_file("SITE.erl", FromPath, ToPath, Options) ->
    % Replace with the site name
    ToPath1 = filename:join([
                    filename:dirname(ToPath),
                    z_convert:to_list(proplists:get_value(site, Options))++".erl"
                ]),
    case filelib:is_file(ToPath1) of
        true ->
            ok;
        false ->
            % Replace the module name, write the site Erlang module.
            Outfile = replace_tags(file:read_file(FromPath), Options),
            file:write_file(ToPath1, Outfile)
    end;
copy_file("config.in", FromPath, ToPath, Options) ->
    ToPath1 = filename:join([filename:dirname(ToPath), "config"]),
    ToConfig = case filelib:is_file(ToPath1) of
        true -> file:consult(ToPath1);
        false -> []
    end,
    FromConfig = file:consult(FromPath),
    MergedConfigs = lists:keymerge(1, lists:sort(ToConfig), lists:sort(FromConfig)),
    Options1 = proplists:delete(site, Options),
    Options2 = normalize_options(Options1),
    FinalConfig = lists:keymerge(1, lists:sort(Options2), MergedConfigs),
    case file:write_file(ToPath1, io_lib:format("~p.", [FinalConfig])) of
        ok ->
            % Also copy the config.in file
            file:copy(FromPath, ToPath),
            ok;
        {error, _} = Error ->
            lager:error("[zotonic_status] Error writing ~p: ~p",
                [ToPath1, Error]),
            Error
    end;
copy_file(_Filename, FromPath, ToPath, _Options) ->
    case filelib:is_file(ToPath) of
        true -> ok;
        false -> file:copy(FromPath, ToPath)
    end.


normalize_options(Options) ->
    lists:map(
            fun
                ({dbdatabase, <<"none">>}) ->
                    {dbdatabase, none};
                ({skeleton, Skel}) ->
                    {skeleton, z_convert:to_atom(Skel)};
                ({K,V}) when is_binary(V) ->
                    {K, binary_to_list(V)};
                (KV) ->
                    KV
            end,
            Options).


site_dir(Name) ->
    z_convert:to_list(filename:join([z_path:sites_dir(), Name])).

skel_dir(Options) ->
    case z_string:to_name(proplists:get_value(skeleton, Options, <<"empty">>)) of
        <<>> -> undefined;
        <<"_">> -> undefined;
        Skel -> filename:join([z_utils:lib_dir(priv), "skel", Skel])
    end.

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

replace_tags(Bin, Options) when is_binary(Bin) ->
    Parts = re:split(Bin, "(%%[A-Z]+%%)", [{return,binary}]),
    lists:map(
            fun(P) -> 
                z_convert:to_binary(map_tag(P, Options))
            end,
            Parts).

map_tag(<<"%%SITE%%">>, Options) -> proplists:get_value(site, Options);
map_tag(<<"%%SITEHOSTNAME%%">>, Options) -> proplists:get_value(hostname, Options);
map_tag(<<"%%SKEL%%">>, Options) -> proplists:get_value(skeleton, Options);
map_tag(<<"%%FULLNAME%%">>, _Options) -> <<>>;
map_tag(<<"%%DBHOST%%">>, Options) -> proplists:get_value(dbhost, Options);
map_tag(<<"%%DBPORT%%">>, Options) -> proplists:get_value(dbport, Options);
map_tag(<<"%%DBUSER%%">>, Options) -> proplists:get_value(dbuser, Options);
map_tag(<<"%%DBPASSWORD%%">>, Options) -> proplists:get_value(dbpassword, Options);
map_tag(<<"%%DBDATABASE%%">>, Options) -> proplists:get_value(dbdatabase, Options);
map_tag(<<"%%DBSCHEMA%%">>, Options) -> proplists:get_value(dbschema, Options);
map_tag(<<"%%ADMINPASSWORD%%">>, Options) -> proplists:get_value(admin_password, Options);
map_tag(<<"%%YEAR%%">>, _Options) ->  z_dateformat:format(calendar:local_time(), "Y", []);
map_tag(<<"%%DATE%%">>, _Options) -> z_dateformat:format(calendar:local_time(), "Y-m-d", []);
map_tag(_, _Options) -> <<>>.


get_fallback(Opt, Options, Default) ->
    case proplists:get_value(Opt, Options, Default) of
        <<>> -> Default;
        undefined -> Default;
        Val -> Val
    end.

check_name(Name, Context) ->
    case is_reserved(Name) of
        true ->
            {error, ?__("This name is reserved", Context)};
        false ->
            Name = binary_to_atom(Name, utf8),
            case is_module(Name) of
                true -> {error, ?__("This name is taken by another module", Context)};
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


error(eacces, SiteDir, Context) ->
    {error, [   ?__("No permission to create the site directory at:", Context),
                " ", SiteDir
            ]};
error(eexist, SiteDir, Context) ->
    {error, [   ?__("The file or directory already exists:", Context),
                " ", SiteDir
            ]};
error(enoent, SiteDir, Context) ->
    {error, [   ?__("The parent directory does not exist:", Context),
                " ", SiteDir
            ]};
error(enospc, SiteDir, Context) ->
    {error, [   ?__("Disk full creating:", Context),
                " ", SiteDir
            ]};
error(enotdir, SiteDir, Context) ->
    {error, [   ?__("A component of the path is not a directory:", Context),
                " ", SiteDir
            ]};
error(Error, SiteDir, Context) ->
    {error, [   ?__("Could not create the file:", Context),
                " ", SiteDir, " (", z_convert:to_binary(Error), ")"
            ]}.
