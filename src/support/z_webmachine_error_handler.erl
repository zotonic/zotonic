%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Error handler for webmachine HTTP errors. The result varies depending on the content type being served.
%% @todo Mail the error to the webadmin

%% Copyright 2009 Marc Worrell, Arjan Scherpenisse
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

-module(z_webmachine_error_handler).
-author("Marc Worrell <marc@worrell.nl>").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([render_error/3]).

-include_lib("zotonic.hrl").


render_error(Code, ReqData, _Reason) when Code == 403; Code == 404 ->
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("Resource not found: ~p", [wrq:raw_path(ReqData)]))),
    Type = webmachine_request:get_metadata('content-type', ReqData),
    error_handler(Type, ReqData, Code, ErrorDump);

render_error(Code=500, ReqData, Reason) ->
    error_logger:error_msg("webmachine error: path=~p~n~p~n", [wrq:path(ReqData), Reason]),
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("~p", [Reason]))),
    Type = webmachine_request:get_metadata('content-type', ReqData),
    error_handler(Type, ReqData, Code, ErrorDump).


error_handler("application/json", ReqData, Code, ErrorDump) ->
    RD1 = wrq:set_resp_header("Content-Type", "application/json; charset=utf-8", ReqData),
    RD2 = wrq:set_resp_header("Content-Encoding", "identity", RD1),
    JS = {struct, [{error_code, Code}, {error_dump, ErrorDump}]},
    {mochijson:encode(JS), RD2};

error_handler(_Default, ReqData, Code, ErrorDump) ->
    RD1 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", ReqData),
    RD2 = wrq:set_resp_header("Content-Encoding", "identity", RD1),
    Host = webmachine_request:get_metadata('zotonic_host', RD2),
    try 
        Context = z_context:new(Host),
        Vars = [
            {error_code, Code}, 
            {error_dump, ErrorDump}
        ],
        Html = z_template:render("error.tpl", Vars, Context),
        {Output, _} = z_context:output(Html, Context),
        {Output, RD2}
    catch
        _:_ -> {<<>>,RD2}
    end.

