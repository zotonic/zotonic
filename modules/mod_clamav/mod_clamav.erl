-module(mod_clamav).

-mod_title("ClamAV").
-mod_description("Scan uploaded files for viruses and malware.").
-mod_prio(100).

-export([
     observe_media_upload_preprocess/2,
     observe_tick_1h/2
]).

-include("zotonic.hrl").


%% @doc Check the uploaded file with clamav
observe_media_upload_preprocess(#media_upload_preprocess{ file = File } = Pre, Context) ->
    case z_clamav:scan_file(File) of
        ok ->
            % all ok, give the next preprocessor a try
            lager:info("clamav: file ~p for user ~p is ok",
                       [ Pre#media_upload_preprocess.original_filename,
                         z_acl:user(Context)
                       ]),
            undefined;
        {error, _} = Error ->
            lager:error("clamav: error ~p checking ~p (~p) for user ~p",
                        [ Error,
                          Pre#media_upload_preprocess.original_filename,
                          Pre#media_upload_preprocess.mime,
                          z_acl:user(Context)
                        ]),
            Error
    end.


%% @doc Periodic ping of clamav to check the settings
observe_tick_1h(tick_1h, _Context) ->
    {IP, Port} = z_clamav:ip_port(),
    case z_clamav:ping() of
        pong ->
            lager:info("clamav: ping ok for clamav daemon at ~s:~p",
                        [ IP, Port ]);
        pang ->
            lager:error("clamav: can't ping clamav daemon at ~s:~p",
                        [ IP, Port ])
    end.
