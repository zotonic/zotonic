%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell <marc@worrell.nl>
%% @doc Upload files to the dropbox folder

%% Copyright 2020 Marc Worrell
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

-module(controller_dropbox_upload).

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
]).


-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    Context1 = z_context:continue_session(Context),
    ?WM_REPLY(true, Context1).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain", undefined}], ReqData, Context}.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(Context1),
    Secret = z_convert:to_binary( m_config:get_value(site, dropbox_upload_secret, Context2) ),
    case z_convert:to_binary( z_context:get_q("secret", Context2)) of
        Secret when Secret =/= <<>> ->
            case handle_upload(Context2) of
                ok -> resp(<<"ok">>, Context2);
                _ -> resp(<<"error">>, Context2)
            end;
        Other ->
            lager:error("Dropbox refusing upload as ~p is not the expected secret.", [ Other ]),
            ?WM_REPLY({halt, 401}, Context2)
    end.

resp(Data, Context) ->
    {x, RD, Ctx1} = ?WM_REPLY(x, Context),
    RD1 = wrq:set_resp_body(Data, RD),
    {{halt, 200}, RD1, Ctx1}.


handle_upload(Context) ->
    Qs = z_context:get_q_all_noz(Context),
    do_upload(Qs, Context).

do_upload([], _Context) ->
    ok;
do_upload([ {_, #upload{ filename = Filename, tmpfile = TempFile }} | Rest ], Context) ->
    Filename1 = sanitize_filename( z_convert:to_binary(Filename), <<>> ),
    DefaultDropBoxDir = z_path:files_subdir_ensure("dropbox", Context),
    DropboxDir = case m_site:get(dropbox_dir, Context) of
        undefined -> DefaultDropBoxDir;
        <<>> -> DefaultDropBoxDir;
        "" -> DefaultDropBoxDir;
        Dir -> Dir
    end,
    Target = filename:join(DropboxDir, Filename1),
    lager:info("Dropbox accepting file ~p to ~p", [ Filename1, Target ]),
    case file:rename(TempFile, Target) of
        ok ->
            do_upload(Rest, Context);
        {error, exdev} ->
            case file:copy(DropboxDir, Filename1) of
                {ok, _} ->
                    do_upload(Rest, Context);
                {error, Reason} ->
                    lager:error("Dropbox error copying file from ~p to ~p: ~p", [ TempFile, Target, Reason ]),
                    {error, Reason}
            end;
        {error, Reason} ->
            lager:error("Dropbox error moving file from ~p to ~p: ~p", [ TempFile, Target, Reason ]),
            {error, Reason}
    end;
do_upload([ _ | Rest ], Context) ->
    do_upload(Rest, Context).

sanitize_filename(<<>>, <<>>) ->
    <<"untitled">>;
sanitize_filename(<<>>, Acc) ->
    Acc;
sanitize_filename(<<C/utf8, Rest/binary>>, Acc) when C >= $a, C =< $z ->
    sanitize_filename(Rest, <<Acc/binary, C/utf8>>);
sanitize_filename(<<C/utf8, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    sanitize_filename(Rest, <<Acc/binary, C/utf8>>);
sanitize_filename(<<C/utf8, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    sanitize_filename(Rest, <<Acc/binary, C/utf8>>);
sanitize_filename(<<C/utf8, Rest/binary>>, Acc) when C =:= $.; C =:= $_; C =:= $- ->
    sanitize_filename(Rest, <<Acc/binary, C/utf8>>);
sanitize_filename(<<_/utf8, Rest/binary>>, Acc) ->
    sanitize_filename(Rest, <<Acc/binary, "_">>).

