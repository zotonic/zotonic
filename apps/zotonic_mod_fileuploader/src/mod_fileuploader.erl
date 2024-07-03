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
-export([start_child/3, start_child/4]).
-export([observe_validate_query_args/3]).

%% Supervisor callbacks
-export([init/1]).

-export([event/2]).


%% ===================================================================
%% API functions
%% ===================================================================


%% @doc Upload event from the UI.
event(#z_msg_v1{ data = Data }, Context) ->
    ExtraData = proplists:get_value(<<"data">>, Data),
    Context1 = lists:foldl(
        fun(#{ <<"upload">> := Upload }, Ctx) ->
            do_upload(z_fileuploader:status(Upload), ExtraData, Ctx)
        end,
        Context,
        proplists:get_value(<<"fileuploader">>, Data)),
    z_render:wire({unmask, [ {target, "body"} ]}, Context1).

do_upload({ok, #{ is_complete := true } = Status}, Data, Context) ->
    #{
        name := Name,
        filename := Filename,
        uploaded_file := TmpFile
    } = Status,
    ?LOG_DEBUG(#{
        in => zotonic_mod_fileuploader,
        text => <<"Upload finished with complete file">>,
        result => ok,
        filename => Filename,
        tmp_file => TmpFile
    }),
    RscProps = #{
        <<"is_published">> => true,
        <<"original_filename">> => Filename,
        <<"is_dependent">> => z_convert:to_bool( maybe_get(<<"is_dependent">>, Data, false) )
    },
    RscProps1 = rsc_props_from_data(RscProps, Data, Context),
    Context1 = case m_media:insert_file(TmpFile, RscProps1, [], Context) of
        {ok, RscId} ->
            maybe_edge(RscId, Data, Context),
            F = z_html:escape(Filename),
            z_render:growl([ ?__("Uploaded", Context), " ", F ], Context);
        {error, _} ->
            F = z_html:escape(Filename),
            z_render:growl_error([ ?__("Could not upload", Context), " ", F ], Context)
    end,
    z_fileuploader:stop(Name),
    Context1;
do_upload({ok, #{ is_complete := false, filename := Filename, uploaded_file := TmpFile }}, _Data, Context) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_fileuploader,
        text => <<"Upload finished with incomplete file">>,
        result => error,
        reason => incomplete,
        filename => Filename,
        tmp_file => TmpFile
    }),
    Context;
do_upload({error, Reason}, _Data, Context) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_fileuploader,
        text => <<"Error uploading file">>,
        result => error,
        reason => Reason
    }),
    Context.

maybe_get(K, M, D) when is_map(M) -> maps:get(K, M, D);
maybe_get(_, _, D) -> D.

rsc_props_from_data(RscProps, #{ <<"subject_id">> := Id }, Context) ->
    RscProps#{
        <<"content_group_id">> => m_rsc:p(Id, content_group_id, Context)
    };
rsc_props_from_data(RscProps, #{ <<"object_id">> := Id }, Context) ->
    RscProps#{
        <<"content_group_id">> => m_rsc:p(Id, content_group_id, Context)
    };
rsc_props_from_data(RscProps, _Data, _Context) ->
    RscProps.

maybe_edge(RscId, #{ <<"subject_id">> := Id } = Data, Context) ->
    Predicate = maps:get(<<"predicate">>, Data, <<"relation">>),
    m_edge:insert(Id, Predicate, RscId, Context);
maybe_edge(RscId, #{ <<"object_id">> := Id } = Data, Context) ->
    Predicate = maps:get(<<"predicate">>, Data, <<"relation">>),
    m_edge:insert(RscId, Predicate, Id, Context);
maybe_edge(_RscId, _Data, _Context) ->
    ok.


%% @doc Start the simple_one_for_one supervisor for the file upload processes.
start_link(Args) ->
    jsxrecord:load_records(z_fileuploader_recorddefs),
    {context, Context} = proplists:lookup(context, Args),
    SupName = z_utils:name_for_site(?MODULE, Context),
    supervisor:start_link({local, SupName}, ?MODULE, []).


%% @doc Start a file uploader.
-spec start_child( Filename :: binary(), Size :: non_neg_integer(), Context :: z:context() ) -> {ok, map()} | {error, term()}.
start_child(Filename, Size, Context) ->
    start_child(z_ids:id(), Filename, Size, Context).

%% @doc Start a file uploader.
-spec start_child( Name :: binary(), Filename :: binary(), Size :: non_neg_integer(), Context :: z:context() ) -> {ok, map()} | {error, term()}.
start_child(Name, Filename, Size, Context) ->
    SupName = z_utils:name_for_site(?MODULE, Context),
    ContextSpawn = z_context:prune_for_spawn(Context),
    case supervisor:start_child(SupName, [Name, Filename, Size, ContextSpawn]) of
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
