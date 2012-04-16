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
    ua_probe/2,
    ua_select/2,
    to_ua_class/1,
    filename_split_class/1,
    order_class/2,
    classes/0
]).

-include_lib("zotonic.hrl").

-define(UA_COOKIE, "z_ua").
-define(UA_COOKIE_MAX_AGE, 3600*24*3650).

-type ua_template_class() :: ua_classifier:device_type() | generic.

%% @doc Classify the user agent in the wm_reqdata.
-spec set_class(#wm_reqdata{}) -> {ok, #wm_reqdata{}}.
set_class(#wm_reqdata{} = ReqData) ->
    {UAClass, UAProps, IsUserSelect} = derive_class_from_reqdata(ReqData),
    {ok, RD1} = webmachine_request:set_metadata(
                    ua_class,
                    UAClass,
                    ReqData),
    webmachine_request:set_metadata(
            ua_props,
            [
                {is_user_select, IsUserSelect},
                {is_touch, has_touchscreen(UAProps)}
                | UAProps
            ],
            RD1).

    %% @doc Try to find the user agent class from the ua classifier and the ua cookie.
    derive_class_from_reqdata(ReqData) ->   
        {UAClass, UAProps} = get_ua_header(ReqData),
        case get_cookie(ReqData) of
            {UAClassCookie, UAPropsCookie, IsUserDefined} ->
                % Cookie with result of previous tests, merge with classifier props
                {UAClassCookie, 
                 lists:foldl(fun({K,V}, Acc) ->
                                lists:keyreplace(K, 1, Acc, {K,V})
                             end,
                             UAProps,
                             UAPropsCookie),
                 IsUserDefined};
            undefined ->
                {UAClass, UAProps, false}
        end.

    %% @doc Fetch the ua classification from a cookie, if present.
    get_cookie(ReqData) ->
        case webmachine_request:get_cookie_value(?UA_COOKIE, ReqData) of
            undefined ->
                undefined;
            Cookie ->
                Qs = mochiweb_util:parse_qs(mochiweb_util:unquote(Cookie)),
                case to_ua_class(proplists:get_value("c", Qs)) of
                    undefined ->
                        undefined;
                    U ->
                        try
                            IsUserDefined = z_convert:to_bool(proplists:get_value("u", Qs)),
                            Props = lists:foldl(fun({"h",V}, Acc) -> [{displayHeight, list_to_integer(V)} | Acc];
                                                   ({"w",V}, Acc) -> [{displayWidth, list_to_integer(V)} | Acc];
                                                   (_,Acc) -> Acc
                                                 end,
                                                 [],
                                                 Qs),
                            {U, Props, IsUserDefined}
                        catch
                            _:_ ->
                                undefined
                        end
                end
        end.
        

    %% @doc Let the ua-classifier do its work on the user-agent request header.
    get_ua_header(ReqData) -> 
        case wrq:get_req_header_lc("user-agent", ReqData) of
            undefined ->
                {desktop, []};
            UserAgent ->
                case ua_classifier:classify(UserAgent) of
                    {ok, Props} ->
                        {ua_classifier:device_type(Props), Props};
                    {error, _Reason} ->
                        {desktop, []}
                end
        end.

to_ua_class("desktop") -> desktop;
to_ua_class("tablet") -> tablet;
to_ua_class("phone") -> phone;
to_ua_class("text") -> text;
to_ua_class(_) -> undefined.

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

%% @doc The user selects an user agent by hand. Update cookie and session.
-spec ua_select(ua_classifier:device_type() | automatic, #context{}) -> #context{}.
ua_select(automatic, Context) ->
    {UAClass, UAProps} = get_ua_header(z_context:get_reqdata(Context)),
    CurrWidth = proplists:get_value(displayWidth, UAProps, 800),
    CurrHeight = proplists:get_value(displayHeight, UAProps, 600),
    CurrIsTouch = has_touchscreen(UAProps),
    set_cookie(UAClass,
               false,
               CurrIsTouch,
               CurrWidth,
               CurrHeight,
               Context);
ua_select(UAClass, Context) ->
    case get_class(Context) of
        UAClass ->
            Context;
        _Other ->
            CurrPs = get_props(Context),
            CurrWidth = proplists:get_value(displayWidth, CurrPs, 800),
            CurrHeight = proplists:get_value(displayHeight, CurrPs, 600),
            CurrIsTouch = has_touchscreen(CurrPs),
            set_cookie(UAClass,
                       true,
                       CurrIsTouch,
                       CurrWidth,
                       CurrHeight,
                       Context)
    end.

%% @doc Handle the return value of the user-agent probe.
%%      When it comes back we know the user-agent has javascript, so we might want to upgrade
%%      from text to phone.
ua_probe(ProbePs, Context) ->
    ua_probe(false, ProbePs, Context).

ua_probe(SetManual, ProbePs, Context) ->
    CurrPs = get_props(Context),
    CurrClass = get_class(Context),

    CurrIsUser = not SetManual orelse proplists:get_value(is_user_select, CurrPs, false),
    CurrWidth = proplists:get_value(displayWidth, CurrPs, 800),
    CurrHeight = proplists:get_value(displayHeight, CurrPs, 600),
    CurrIsTouch = has_touchscreen(CurrPs),

    ProbeWidth = proplists:get_value(width, ProbePs),
    ProbeHeight = proplists:get_value(height, ProbePs),
    ProbeIsTouch = proplists:get_value(is_touch, ProbePs, false),

    IsChanged = CurrWidth =/= ProbeWidth
            orelse CurrHeight =/= ProbeHeight
            orelse CurrIsTouch =/= ProbeIsTouch,
            
    case CurrIsUser of
        false ->
            % As we got a probe back we know JS is working.
            % Upgrade the 'text' classification to 'phone'
            NewClass = class_from_size(CurrClass, ProbeWidth, ProbeHeight, ProbeIsTouch),
            case NewClass =/= CurrClass of
                false ->
                    case IsChanged of
                        true ->
                            {ok, set_cookie(CurrClass,
                                            false,
                                            ProbeIsTouch,
                                            ProbeWidth,
                                            ProbeHeight,
                                            Context)};
                        false ->
                            ok
                    end;
                true ->
                    {reload, set_cookie(NewClass,
                                        false,
                                        ProbeIsTouch,
                                        ProbeWidth,
                                        ProbeHeight,
                                        Context)}
            end;
        true ->
            % User defined: don't change the user agent class
            case IsChanged of
                true ->
                    {ok, set_cookie(CurrClass,
                                    true,
                                    ProbeIsTouch,
                                    ProbeWidth,
                                    ProbeHeight,
                                    Context)};
                false ->
                    ok
            end
    end.

    % Set the cookie, update the session with the new ua props
    set_cookie(Class, IsUser, IsTouch, W, H, Context) ->
        % Keep this sorted on key
        Props = [
            {displayHeight, H},
            {displayWidth, W},
            {is_touch, IsTouch},
            {is_user_select, IsUser}
        ],
        z_session:set(ua_class, Class, Context),
        z_session:set(ua_props, lists:keymerge(1, Props, lists:keysort(1, get_props(Context))), Context),
        V = mochiweb_util:urlencode([
                            {"c", Class},
                            {"u", case IsUser of true -> 1; false -> 0 end},
                            {"t", case IsTouch of true -> 1; false -> 0 end},
                            {"w", W},
                            {"h", H}
                        ]),
        Options = [
            {max_age, ?UA_COOKIE_MAX_AGE}, 
            {path, "/"},
            {http_only, false}
        ],
        z_context:set_cookie(?UA_COOKIE, mochiweb_util:quote_plus(V), Options, Context).


    % Get the class from the user agent screen size and touch input.
    class_from_size(_, W, H, false) when W >= 800, H >= 480 -> desktop;
    class_from_size(_, W, H, true) when W >= 800, H >= 480 -> tablet;
    class_from_size(_, W, H, _IsTouch) when W =< 800; H =< 480 -> phone;
    class_from_size(C, _, _, _) -> C.


has_touchscreen(UAProps) ->
    case proplists:get_value(inputDevices, UAProps) of
       Ds when is_list(Ds) -> lists:member(<<"touchscreen">>, Ds);
       _ -> false
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
