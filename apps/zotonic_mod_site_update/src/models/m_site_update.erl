%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2026 Marc Worrell
%% @doc Fetch the status of the sites.
%% @end

%% Copyright 2019-2026 Marc Worrell
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

-module(m_site_update).
-moduledoc("
Model for checking if a site has version control enabled.

Also implements the webhook to force a pull of new code from the version control system.

The URL for the webhook is:


```erlang
https://yoursite.test/api/model/site_update/post/webhook/<token>
```

Where `<token\\>` should be replaced with your configured token.

The token can be configured in the admin on System > Modules and then the config of the mod_site_update module.

The token is saved in the config key `mod_site_update.webhook_token`.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/is_vcs_site/...` | Return whether the current site source directory resolves to a VCS checkout (`.git`, `.hg`, or symlinked checkout). |
| `get` | `/vcs_site/+site/...` | Return admin-only VCS tuple for site `+site` (`{git|hg, Path}`) or `false` when no repository is detected. |
| `get` | `/vcs_zotonic/...` | Return admin-only VCS tuple for the Zotonic core library directory (`{git|hg, Path}`) or `false` when not under VCS. |
| `post` | `/webhook` | Read webhook token from query arg `token`; if it matches `mod_site_update.webhook_token`, run VCS update (`git pull`/`hg pull -u`) for the current site. No further lookups. |
| `post` | `/webhook/+token` | Use path token `+token`; if it matches `mod_site_update.webhook_token`, run VCS update (`git pull`/`hg pull -u`) for the current site. No further lookups. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    m_post/3,

    vcs_zotonic/0,
    vcs_site/1,
    vcs_dir/1,

    update_vcs/2
]).

-type vcs() :: {git | hg, file:filename_all()}.

-export_type([vcs/0]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"is_vcs_site">> | Rest ], _Msg, Context) ->
    Site = z_context:site(Context),
    IsVCS = case vcs_site(Site) of
        false -> false;
        {_, _} -> true
    end,
    {ok, {IsVCS, Rest}};
m_get([ <<"vcs_site">>, Site | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {vcs_site(Site), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"vcs_zotonic">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {vcs_zotonic(), Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


m_post([ <<"webhook">> ], _Msg, Context) ->
    Token = z_context:get_q(<<"token">>, Context),
    webhook(Token, Context);
m_post([ <<"webhook">>, Token ], _Msg, Context) ->
    webhook(Token, Context).



webhook(undefined, _Context) ->
    {error, eacces};
webhook(<<>>, _Context) ->
    {error, eacces};
webhook(Token, Context) ->
    OurToken = m_config:get_value(mod_site_update, webhook_token, Context),
    case Token of
        OurToken when OurToken =/= <<>> ->
            ?zInfo("Received site update webhook request.", Context),
            _ = update_vcs( vcs_site( z_context:site(Context) ), Context ),
            {ok, <<"Updating site.">>};
        _ ->
            ?zInfo("Received site update webhook request, but access denied.", Context),
            {error, eacces}
    end.


%% @doc Run the VCS update command for a directory.
update_vcs(false, _Context) ->
    {error, no_vcs};
update_vcs({hg, Path}, Context) ->
    Command = lists:flatten(["(cd ", z_filelib:os_filename(Path), "; hg pull -u)"]),
    Result = os:cmd(Command),
    zotonic_fileindexer:flush(),
    ?zInfo("hg pull: ~s", [ Result ], Context),
    {ok, Result};
update_vcs({git, Path}, Context) ->
    Command = lists:flatten(["(cd ", z_filelib:os_filename(Path), "; git pull)"]),
    Result = os:cmd(Command),
    zotonic_fileindexer:flush(),
    ?zInfo("git pull: ~s", [ Result ], Context),
    {ok, Result}.


%% @doc Check if zotonic itself has a .hg directory
-spec vcs_zotonic() -> vcs() | false.
vcs_zotonic() ->
    vcs_dir( z_utils:lib_dir() ).

% @doc Check if the site directory has a mercurial .hg subdirectory
-spec vcs_site( atom() | binary() | z:context() ) -> vcs() | false.
vcs_site(zotonic_status) ->
    vcs_zotonic();
vcs_site(Site) when is_binary(Site) ->
    try
        SiteAtom = erlang:binary_to_existing_atom(Site, utf8),
        vcs_site(SiteAtom)
    catch
        error:badarg ->
            false
    end;
vcs_site(Site) when is_atom(Site) ->
    case z_path:site_source_dir(Site) of
        {error, bad_name} -> false;
        Path -> vcs_dir(Path)
    end.

-spec vcs_dir( file:filename_all() ) -> vcs() | false.
vcs_dir(Dir) ->
    VCSs = [
        {".git", {git, Dir}},
        {".hg",  {hg, Dir}}
    ],
    case file:read_link_info(Dir) of
        {ok, #file_info{ type = symlink }} ->
            % Assume it is a git directory.
            % It is usual to deep-link to a checkout from the app_user directory.
            {git, Dir};
        {ok, #file_info{ type = directory }} ->
            lists:foldl(
                fun
                    ({DotDir, VCS}, false) ->
                        case filelib:is_dir(filename:join([Dir, DotDir]))  of
                            true -> VCS;
                            false -> false
                        end;
                    (_, Found) ->
                        Found
                end,
                false,
                VCSs)
    end.
