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
    get_props/1,
    filename_split_class/1,
    order_class/2,
    classes/0
]).

-include_lib("zotonic.hrl").

-type ua_template_class() :: ua_classifier:device_type() | generic.

%% @doc Classify the user agent in the wm_reqdata.
-spec set_class(#wm_reqdata{}) -> {ok, #wm_reqdata{}}.
set_class(#wm_reqdata{} = ReqData) ->
    case wrq:get_req_header_lc("user-agent", ReqData) of
        undefined ->
            webmachine_request:set_metadata(
                ua_class,
                desktop,
                ReqData);
        UserAgent ->
            case ua_classifier:classify(UserAgent) of
                {ok, Props} ->
                    {ok, RD1} = webmachine_request:set_metadata(
                                    ua_class,
                                    ua_classifier:device_type(Props),
                                    ReqData),
                    webmachine_request:set_metadata(
                            ua_props,
                            Props,
                            RD1);
                {error, _Reason} ->
                    webmachine_request:set_metadata(
                        ua_class,
                        desktop,
                        ReqData)
            end
    end.

%% @doc Get the user agent class
-spec get_class( #context{} | #wm_reqdata{} | undefined ) -> ua_classifier:device_type().
get_class(undefined) ->
    desktop;
get_class(#context{} = Context) ->
    get_class(z_context:get_reqdata(Context));
get_class(#wm_reqdata{} = ReqData) ->
    case webmachine_request:get_metadata(ua_class, ReqData) of
        undefined -> desktop;
        UAClass -> UAClass
    end.

%% @doc Get the user agent class
-spec get_props( #context{} | #wm_reqdata{} | undefined ) -> [ {atom(), term()} ].
get_props(undefined) ->
    [{id, <<"desktop">>}];
get_props(#context{} = Context) ->
    get_props(z_context:get_reqdata(Context));
get_props(#wm_reqdata{} = ReqData) ->
    case webmachine_request:get_metadata(ua_props, ReqData) of
        undefined -> [{id, <<"desktop">>}];
        Props -> Props
    end.


%% @doc Split a filename (with subdir) to a device class and a relative path.
-spec filename_split_class(file:filename()) -> { ua_template_class(), file:filename() }.
filename_split_class(F) ->
    case filename:split(F) of
        ["desktop"|T] -> {desktop, string:join(T, "/")};
        ["tablet"|T] -> {tablet, string:join(T, "/")};
        ["phone"|T] -> {phone, string:join(T, "/")};
        ["text"|T] -> {text, string:join(T, "/")};
        Parts -> {generic, string:join(Parts, "/")}
    end.


%% @doc Ordering function for lists:sort/2. Orders by genericity. Most specific first.
-spec order_class( ua_classifier:device_type(), ua_classifier:device_type() ) -> boolean().
order_class(A,A) -> true;
order_class(_, generic) -> false;
order_class(desktop, _) -> true;
order_class(phone, text) -> true;
order_class(tablet, text) -> true;
order_class(tablet, phone) -> true;
order_class(_, _) -> false.


%% @doc Return all possible UA classes
-spec classes() -> [ ua_classifier:device_type() ].
classes() ->
    [ text, phone, tablet, desktop ].
