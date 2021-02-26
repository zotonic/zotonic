%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Supervisor for the uploading files.

%% Copyright 2021 Marc Worrell
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

-module(mod_fileuploader).

-author("Marc Worrell <marc@worrell.nl>").
-mod_title("File Uploader").
-mod_description("Support uploading files via MQTT").
-mod_prio(500).
-mod_provides([fileuploader]).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/fileuploader.hrl").

%% API
-export([start_link/1]).
-export([start_child/3]).
-export([observe_validate_query_args/3]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start the simple_one_for_one supervisor for the file upload processes.
start_link(Args) ->
    jsxrecord:load_records(z_fileuploader_recorddefs),
    {context, Context} = proplists:lookup(context, Args),
    SupName = z_utils:name_for_site(?MODULE, Context),
    supervisor:start_link({local, SupName}, ?MODULE, []).


%% @doc Start a file uploader.
-spec start_child( Filename :: binary(), Size :: non_neg_integer(), Context :: z:context() ) -> {ok, map()} | {error, term()}.
start_child(Filename, Size, Context) ->
    SupName = z_utils:name_for_site(?MODULE, Context),
    ContextSpawn = z_context:prune_for_spawn(Context),
    case supervisor:start_child(SupName, [Filename, Size, ContextSpawn]) of
        {ok, Pid} ->
            z_fileuploader:status(Pid);
        {error, _} = Error ->
            Error
    end.

%% @doc Replace all #fileuploader{} records with #upload{} and the uploaded file.
observe_validate_query_args(#validate_query_args{}, {ok, Qs}, _Context) ->
    lists:foldr(
        fun
            ({K, #fileuploader{ name = Name }}, {ok, Acc}) ->
                case z_fileuploader:status(Name) of
                    {ok, #{
                        is_complete := true,
                        filename := Filename,
                        uploaded_file := TmpFile
                    }} ->
                        {ok, {MonitorPid, NewTmpFile}} = z_tempfile:monitored_new(),
                        ok = file:rename(TmpFile, NewTmpFile),
                        z_fileuploader:stop(Name),
                        Upload = #upload{
                            filename = Filename,
                            tmpfile = NewTmpFile,
                            tmpmonitor = MonitorPid,
                            mime = undefined,
                            data = undefined
                        },
                        {ok, [ {K, Upload} | Acc ]};
                    {ok, #{
                        is_complete := false
                    }} ->
                        {error, file_incomplete};
                    {error, _} ->
                        {error, file_not_found}
                end;
            (KV, {ok, Acc}) ->
                {ok, [ KV | Acc ]};
            (_, {error, _} = Err) ->
                Err
        end,
        {ok, []},
        Qs);
observe_validate_query_args(#validate_query_args{}, {error, _} = Error, _Context) ->
    Error.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Supervisor callback, initialize the children.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },
    ChildSpecs = [
        #{
            id => uploader,
            start => {z_fileuploader, start_link, []},
            shutdown => brutal_kill,
            restart => temporary
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
