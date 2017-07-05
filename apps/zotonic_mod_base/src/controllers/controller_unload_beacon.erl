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
    allowed_methods/1,
    process_post/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(unload_beacon, {
    session_id,
    page_id
}).

allowed_methods(Context) ->
    {[<<"POST">>], Context}.

process_post(Context) ->
    case z_context:get_req_header(<<"content-type">>, Context) of
        <<"text/x-ubf", _/binary>> ->
            process_post_ubf(Context);
        <<"text/plain", _/binary>> ->
            process_post_ubf(Context);
        _ ->
            {{halt, 415}, Context}
    end.

%% @doc Process beacon, the received data is UBF.
process_post_ubf(Context) ->
    {Data, Context1} = cowmachine_req:req_body(Context),
    try
        {ok, #unload_beacon{page_id=PageId}, _Rest} = z_transport:data_decode(Data),
        case z_session_page:whereis(PageId, Context) of
            undefined -> ok;
            Pid -> ok = z_session_page:stop(Pid)
        end
    catch Class:Term ->
 	%% Log error, but give a correct response anyway
	lager:error("mod_base error processing unload beacon ~p:~p", [Class, Term])
    end,
    {true, Context1}.
