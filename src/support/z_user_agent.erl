%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc User agent classifications

%% Copyright 2012 Marc Worrell
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

-module(z_user_agent).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    set_class/1,
    get_class/1,
    filename_split_class/1,
    order_class/2
]).

-include_lib("zotonic.hrl").

%% @doc Classify the user agent in the wm_reqdata.
set_class(#wm_reqdata{} = ReqData) ->
    webmachine_request:set_metadata(
        ua_class,
        ua_classifier:classify(wrq:get_req_header_lc("user-agent", ReqData)),
        ReqData).

%% @doc Get the user agent class
get_class(undefined) ->
    desktop;
get_class(#context{} = Context) ->
    get_class(z_context:get_reqdata(Context));
get_class(#wm_reqdata{} = ReqData) ->
    case webmachine_request:get_metadata(ua_class, ReqData) of
        undefined -> desktop;
        UAClass -> UAClass
    end.

%% @doc Split a filename (with subdir) to a device class and a relative path.
filename_split_class(F) ->
    case filename:split(F) of
        ["desktop"|T] -> {desktop, string:join(T, "/")};
        ["tablet"|T] -> {tablet, string:join(T, "/")};
        ["phone"|T] -> {phone, string:join(T, "/")};
        ["text"|T] -> {text, string:join(T, "/")};
        Parts -> {generic, string:join(Parts, "/")}
    end.


%% @doc Ordering function for lists:sort/2. Orders by genericity. Most specific first.
order_class(A,A) -> true;
order_class(_, generic) -> false;
order_class(desktop, _) -> true;
order_class(phone, text) -> true;
order_class(tablet, text) -> true;
order_class(tablet, phone) -> true;
order_class(_, _) -> false.

