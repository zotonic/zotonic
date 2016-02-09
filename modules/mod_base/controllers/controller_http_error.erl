%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Controller for http errors. Called for 4xx errors and serving some expected content.

%% Copyright 2015 Marc Worrell
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

-module(controller_http_error).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    charsets_provided/2,
    content_types_provided/2,
    do_html/2,
    do_text/2,
    do_json/2,
    do_c_comment/2,
    do_empty/2,
    do_image/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    % z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

%% For the content type we perform multiple checks:
%% * Original dispatch rule (think controller_file)
%% * Request path extension
%% * Accept header (todo)
%% Idea is to serve a content type as close as possible
%% to the expected content type.
content_types_provided(ReqData, Context) ->
    Ms = case webmachine_request:get_metadata(controller_module_error, ReqData) of
            % mod_base
            controller_lib ->
                provide_extension(ReqData, provide_text());
            controller_file ->
                provide_extension(ReqData, provide_text());
            controller_file_id ->
                provide_extension(ReqData, provide_text());
            controller_static_pages ->
                provide_extension(ReqData, provide_html());
            controller_page ->
                provide_html();
            controller_template ->
                provide_html();
            controller_redirect ->
                provide_html();
            controller_website_redirect ->
                provide_html();
            controller_id ->
                provide_any();
            controller_api ->
                [
                    {"application/json", do_json},
                    {"application/x-json", do_json},
                    {"text/plain", do_text}
                ];
            % mod_authentication
            controller_logon ->
                provide_html();
            controller_logoff ->
                provide_html();
            % mod_admin
            controller_admin ->
                provide_html();
            controller_admin_edit ->
                provide_html();
            controller_admin_referrers ->
                provide_html();
            controller_media_preview ->
                provide_image();
            % mod_atom
            controller_atom_entry ->
                provide_empty();
            controller_atom_feed_cat ->
                provide_empty();
            controller_atom_feed_search ->
                provide_empty();
            _ ->
                provide_any()
        end,
    {Ms, ReqData, Context}.

provide_any() ->
    [
        {"text/html", do_html},
        {"application/json", do_json},
        {"application/x-json", do_json},
        {"application/javascript", do_c_comment},
        {"application/x-javascript", do_c_comment},
        {"text/css", do_c_comment},
        {"text/plain", do_text},
        {"application/atom+xml;type=entry", do_empty},
        {"application/atom+xml", do_empty}
    ].

provide_extension(ReqData, Default) ->
    case map_extension(ReqData) of
        html ->
            [
                {"text/html", do_html},
                {"text/plain", do_text}
            ];
        json ->
            [
                {"application/json", do_json},
                {"application/x-json", do_json},
                {"text/plain", do_text}
            ];
        css ->
            [
                {"text/css", do_c_comment},
                {"text/plain", do_text}
            ];
        javascript ->
            [
                {"application/javascript", do_c_comment},
                {"application/x-javascript", do_c_comment},
                {"text/plain", do_text}
            ];
        text ->
            [
                {"text/plain", do_text}
            ];
        image ->
            provide_image();
        other ->
            Default
    end.

provide_image() ->
    [
        {"image/gif", do_image}
    ].

provide_html() ->
    [
        {"text/html", do_html},
        {"text/plain", do_text}
    ].

provide_text() ->
    [
        {"text/plain", do_text}
    ].

provide_empty() ->
    [
        {"text/plain", do_empty}
    ].

map_extension(ReqData) ->
    case z_string:to_lower(filename:extension(z_convert:to_binary(wrq:disp_path(ReqData)))) of
        <<>> -> other;
        <<".jpg">> -> image;
        <<".png">> -> image;
        <<".gif">> -> image;
        <<".html">> -> html;
        <<".htm">> -> html;
        <<".js">> -> javascript;
        <<".css">> -> css;
        <<".json">> -> json;
        <<".txt">> -> text;
        _ -> other
    end.

do_html(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    ContextQs = z_context:continue_session(z_context:ensure_qs(Context)),
    Vars = [
        {error_code, webmachine_request:get_metadata(http_status_code, ReqData)}
        | z_context:get_all(ContextQs)
    ],
    Vars1 = case bt_simplify(webmachine_request:get_metadata(error_reason, ReqData)) of
                {reason, ErlangError, Tab} ->
                    [
                        {error_erlang, ErlangError},
                        {error_table, Tab}
                        | Vars
                    ];
                {raw, X} ->
                    [
                        {error_dump, X}
                        | Vars
                    ];
                undefined ->
                    Vars
            end,
    StatusTpl = <<"error.", (z_convert:to_binary(webmachine_request:get_metadata(http_status_code, ReqData)))/binary, ".tpl">>,
    Rendered = case z_module_indexer:find(template, StatusTpl, ContextQs) of
                    {ok, ModuleIndex} -> z_template:render(ModuleIndex, Vars1, ContextQs);
                    {error, enoent} -> z_template:render(<<"error.tpl">>, Vars1, ContextQs)
               end,
    {Output, OutputContext} = z_context:output(Rendered, ContextQs),
    ?WM_REPLY(Output, OutputContext).

do_text(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    Text = httpd_util:reason_phrase(webmachine_request:get_metadata(http_status_code, ReqData)),
    ?WM_REPLY(Text, Context).

do_json(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    StatusCode = webmachine_request:get_metadata(http_status_code, ReqData),
    JSON = iolist_to_binary([
            <<"{ code: ">>, z_convert:to_binary(StatusCode), 
            <<", status: \"">>, httpd_util:reason_phrase(StatusCode), 
            <<"\" }">>
        ]),
    ?WM_REPLY(JSON, Context).

do_c_comment(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    StatusCode = webmachine_request:get_metadata(http_status_code, ReqData),
    JSON = iolist_to_binary([
            <<"/* ">>, z_convert:to_binary(StatusCode), 
            <<" ">>, httpd_util:reason_phrase(StatusCode), 
            <<" */">>
        ]),
    ?WM_REPLY(JSON, Context).

do_empty(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    ?WM_REPLY(<<>>, Context).

do_image(ReqData, Context0) ->
    Context = set_headers(ReqData, Context0),
    ?WM_REPLY(trans_gif(), Context).

set_headers(ReqData, Context) ->
    Context2 = ?WM_REQ(ReqData, Context),
    Context3 = z_context:set_noindex_header(Context2),
    z_context:set_nocache_headers(Context3).

%% 1 pixel transparant gif
trans_gif() ->
    <<71,73,70,56,57,97,1,0,1,0,128,0,0,0,0,0,255,255,255,33,249,4,1,0,0,0,0,44,0,
      0,0,0,1,0,1,0,0,2,1,68,0,59>>.


bt_simplify(undefined) ->
    undefined;
bt_simplify({error, {throw, {error, {template_compile_error, Template, {Line,Col}, Error}}, BT}}) ->
    Msg = case is_list(Error) orelse is_binary(Error) of
            true ->
                io_lib:format("Error compiling template: ~s:~p (~p) ~s", [Template, Line, Col, Error]);
            false ->
                io_lib:format("Error compiling template: ~s:~p (~p) ~s", [Template, Line, Col, Error])
          end,
    {reason, iolist_to_binary(Msg), bt_table(BT)};
bt_simplify({error, {throw, {error, {template_compile_error, _Template, Error}}, BT}}) ->
    {reason, Error, bt_table(BT)};
bt_simplify({_E1, {throw, Reason, BT}}) when is_list(BT) ->
    {reason, stringify(Reason), bt_table(BT)};
bt_simplify({_E, {Reason, BT}}) when is_list(BT) ->
    {reason, stringify(Reason), bt_table(BT)};
bt_simplify(X) ->
    {raw, X}.

stringify(B) when is_binary(B) -> 
    B;
stringify(L) when is_list(L) ->
    try
        list_to_binary(L)
    catch
        _:_ -> io_lib:format("~p", [L])
    end;
stringify(A) when is_atom(A) ->
    binary_to_list(A);
stringify(X) ->
    io_lib:format("~p", [X]).


bt_table(BT) ->
    bt_table(BT, []).

bt_table([], Acc) ->
    lists:reverse(Acc);
bt_table([{Module, Fun, ArityArgs} | T], Acc) ->
    bt_table(T, [bt_row(Module, Fun, ArityArgs, undefined, undefined) | Acc]);
bt_table([{Module, Fun, ArityArgs, Loc} | T], Acc) ->
    bt_table(T, [bt_row(Module, Fun, ArityArgs, proplists:get_value(file, Loc), proplists:get_value(line, Loc)) | Acc]).

bt_row(Module, Fun, ArityArgs, File, Line) ->
    case is_template(atom_to_list(Module)) of
        {true, Mod, Template} ->
            [true, Mod, Template, simplify_args(ArityArgs), loc(File, Line)];
        false ->
            SFun = case is_integer(ArityArgs) of
                        true -> z_convert:to_list(Fun) ++ "/" ++ integer_to_list(ArityArgs);
                        false -> z_convert:to_list(Fun) ++ "/" ++ integer_to_list(length(ArityArgs))
                   end,
            [false, Module, SFun, simplify_args(ArityArgs), loc(File,Line)]
    end.

loc(undefined,undefined) -> undefined;
loc(File,undefined) -> z_convert:to_list(File);
loc(File,Line) -> z_convert:to_list(File)++":"++z_convert:to_list(Line).

simplify_args(N) when is_integer(N) -> undefined;
simplify_args(L) ->
    As = [ simplify_arg(A) || A <- L ],
    iolist_to_binary([$[, z_utils:combine(", ", As), $]]).

    simplify_arg(N) when is_integer(N) -> integer_to_list(N);
    simplify_arg(A) when is_atom(A) -> atom_to_list(A);
    simplify_arg(B) when is_binary(B) -> B;
    simplify_arg([]) -> "[]";
    simplify_arg({}) -> "{}";
    simplify_arg(L) when is_list(L) -> "[...]";
    simplify_arg({A,B}) when is_atom(A), is_atom(B) -> [${,atom_to_list(A),$,,atom_to_list(B),$}];
    simplify_arg(T) when is_tuple(T) ->
        case is_atom(element(1, T)) of
            true -> [$#, atom_to_list(element(1,T)), "{}"];
            false -> io_lib:format("~p", [T])
        end;
    simplify_arg(X) -> io_lib:format("~p", [X]).


is_template("template_"++R) ->
    case re:split(R, "_", [{return,list}]) of
        [_Site,""|Rest] ->
            case lists:last(Rest) of
                "tpl" ->
                    case re:run(R, "modules_(mod_.*)_templates_(.*)_tpl$", [{capture, all, list}]) of
                        {match, [_, Module, Path]} ->
                            {true, Module, Path++".tpl"};
                        nomatch ->
                            case re:run(R, "priv_sites_(.*)_templates_(.*)_tpl$", [{capture, all, list}]) of
                                {match, [_, Module, Path]} ->
                                    {true, Module, Path++".tpl"};
                                nomatch ->
                                    false
                            end
                    end;
                _ -> 
                    false
            end;
        _ ->
            false
    end;
is_template(_) ->
    false.

