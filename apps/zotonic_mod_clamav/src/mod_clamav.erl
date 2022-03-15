%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell

%% @doc Scan uploaded files with clamd.

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

-module(mod_clamav).

-mod_title("ClamAV").
-mod_description("Scan uploaded files for viruses and malware.").
-mod_prio(100).
-mod_provides([ antivirus ]).
-mod_depends([ cron ]).

-export([
     observe_media_upload_preprocess/2,
     observe_tick_1h/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Check the uploaded file with clamav
observe_media_upload_preprocess(#media_upload_preprocess{ file = File, mime = Mime, medium = Medium } = Pre, Context) when is_binary(File) ->
    case maps:get(<<"is_av_sizelimit">>, Medium, false) of
        true ->
            % This is the second try, now with disabled av-scanner
            undefined;
        false ->
            case scan_file(File, Mime, Context) of
                ok ->
                    % all ok, give the next preprocessor a try
                    ?LOG_NOTICE(#{
                        text => <<"clamav: file is ok">>,
                        result => ok,
                        file => Pre#media_upload_preprocess.original_filename
                    }),
                    undefined;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"clamav: file is NOT ok">>,
                        result => error,
                        reason => Reason,
                        file => Pre#media_upload_preprocess.original_filename,
                        mime => Pre#media_upload_preprocess.mime
                    }),
                    UId = z_acl:user(Context),
                    ?zError(
                        "Virus scanner: error '~p' checking '~s' (~s) for user ~p (~s)",
                        [ Reason,
                          Pre#media_upload_preprocess.original_filename,
                          Pre#media_upload_preprocess.mime,
                          UId,
                          z_convert:to_binary( m_rsc:p_no_acl(UId, email, Context) )
                        ],
                        Context),
                    Error
            end
    end;
observe_media_upload_preprocess(#media_upload_preprocess{ file = undefined }, _Context) ->
    undefined.


scan_file(File, Mime, Context) ->
  MimeB = z_convert:to_binary(Mime),
  Fs = [
    fun() -> z_clamav:scan_file(File) end,
    fun() -> z_clamav_msoffice:scan_file(File, MimeB, Context) end
  ],
  lists:foldl(
    fun
        (F, ok) -> F();
        (_F, Err) -> Err
    end,
    ok,
    Fs).


%% @doc Periodic ping of clamav to check the settings
observe_tick_1h(tick_1h, _Context) ->
    {IP, Port} = z_clamav:ip_port(),
    case z_clamav:ping() of
        pong ->
            ?LOG_INFO(#{
                text => <<"Virus scanner: ping ok for clamav">>,
                result => ok,
                ip => IP,
                port => Port
            });
        pang ->
            ?LOG_WARNING(#{
                text => <<"Virus scanner: can not ping clamav daemon">>,
                result => error,
                reason => pang,
                ip => IP,
                port => Port
            })
    end.
