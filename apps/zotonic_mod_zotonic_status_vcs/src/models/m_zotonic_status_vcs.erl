%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Fetch the status of the sites.

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

-module(m_zotonic_status_vcs).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    vcs_zotonic/0,
    vcs_site/1,
    vcs_dir/1
]).

-type vcs() :: {git | hg, file:filename()}.

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ vcs_site, Site | Rest ], _Msg, Context) when is_atom(Site) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {vcs_site(Site), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ vcs_zotonic | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {vcs_zotonic(), Rest}};
        false ->
            {error, eacces}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

%% @doc Check if zotonic itself has a .hg directory
-spec vcs_zotonic() -> vcs() | false.
vcs_zotonic() ->
    vcs_dir( z_utils:lib_dir() ).

% @doc Check if the site directory has a mercurial .hg subdirectory
-spec vcs_site( atom() ) -> vcs() | false.
vcs_site(Site) ->
    vcs_dir( z_path:site_dir(Site) ).

-spec vcs_dir( file:filename() ) -> vcs() | false.
vcs_dir(Dir) ->
    VCSs = [
        {".git", {git, Dir}},
        {".hg",  {hg, Dir}}
    ],
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
        VCSs).
