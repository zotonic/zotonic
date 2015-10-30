%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc Handles posts from the page unload beacon

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(controller_unload_beacon).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1, 
    service_available/2,
    allowed_methods/2,
    process_post/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

-record(unload_beacon, { 
    session_id,
    page_id
}).

init(DispatchArgs) -> 
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    ?WM_REPLY(true, Context).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    case wrq:get_req_header_lc("content-type", ReqData) of
        "text/x-ubf" ++ _ ->
            process_post_ubf(ReqData, Context);
        "text/plain" ++ _ ->
            process_post_ubf(ReqData, Context);
        _ ->
            {{halt, 415}, ReqData, Context}
    end. 

%% @doc Process beacon, the received data is UBF.
process_post_ubf(ReqData, Context) ->
    {Data, RD1} = wrq:req_body(ReqData),
    try 
        {ok, #unload_beacon{page_id=PageId}, _Rest} = z_transport:data_decode(Data),

        case z_session_page:whereis(PageId, Context) of
            undefined -> ok;
            Pid -> ok = z_session_page:stop(Pid)
        end
    catch Class:Term ->
 	%% Log error, but give a correct response anyway
	lager:error("~p: mod_base error processing unload beacon ~p:~p", 
			[z_context:site(Context), Class, Term])
    end,

    {true, RD1, Context}.
