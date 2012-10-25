%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% Date: 2010-09-20
%% @doc Import/export for zotonic.

%% Copyright 2010,2011 Arjan Scherpenisse
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

-module(mod_import_wordpress).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Import WordPress WXR").
-mod_description("Import your Wordpress blog into Zotonic using a .wxr file.").

-include_lib("zotonic.hrl").

%% interface functions
-export([
         event/2,
         do_import/4
]).


event(#submit{message={wxr_upload, []}}, Context) ->
    #upload{filename=OriginalFilename, tmpfile=TmpFile} = z_context:get_q_validated("upload_file", Context),
    Reset = z_convert:to_bool(z_context:get_q("reset", Context)),
    z_session_page:spawn_link(?MODULE, do_import, [TmpFile, Reset, OriginalFilename, Context], Context),

    Context2 = z_render:growl("Please hold on while the file is importing. You will get a notification when it is ready.", Context),
    z_render:wire([{dialog_close, []}], Context2).

do_import(TmpFile, Reset, OriginalFilename, Context) ->
    Context1 = 
        try
            ok = import_wordpress:wxr_import(TmpFile, Reset, Context),
            Msg = lists:flatten(io_lib:format("The import of ~p has completed.", [OriginalFilename])),
            z_render:growl(Msg, Context)
        catch
            _:E ->
                Msg1 = lists:flatten(io_lib:format("~p failed to import. The error was: ~p", [OriginalFilename, E])),
                ?zWarning(Msg1, Context),
                lager:warning("Wordpress error: ~p~n~p", [E, erlang:get_stacktrace()]),
                z_render:growl(Msg1, error, true, Context)
        end,
    z_session_page:add_script(Context1).
