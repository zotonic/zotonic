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
    charsets_provided/1,
    content_types_provided/1,
    do_html/1,
    do_text/1,
    do_json/1,
    do_c_comment/1,
    do_empty/1,
    do_image/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

%% For the content type we perform multiple checks:
%% * Original dispatch rule (think controller_file)
%% * Request path extension
%% * Accept header (todo)
%% Idea is to serve a content type as close as possible
%% to the expected content type.
content_types_provided(Context) ->
    Ms = case cowmachine_req:get_metadata(controller_module_error, Context) of
            % mod_base
            controller_lib ->
                provide_extension(Context, provide_text());
            controller_file ->
                provide_extension(Context, provide_text());
            controller_file_id ->
                provide_extension(Context, provide_text());
            controller_static_pages ->
                provide_extension(Context, provide_html());
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
                    {<<"application/json">>, do_json},
                    {<<"application/x-json">>, do_json},
                    {<<"text/plain">>, do_text}
                ];
            % mod_authentication
            controller_logon ->
                provide_html();
            controller_logoff ->
                provide_html();
            controller_admin ->
                provide_html();
            controller_admin_edit ->
                provide_html();
            controller_admin_referrers ->
                provide_html();
            controller_media_preview ->
                provide_image();
            controller_letsencrypt_challenge ->
                provide_text();
            _ ->
                provide_any()
        end,
    {Ms, Context}.

provide_any() ->
    [
        {<<"text/html">>, do_html},
        {<<"application/json">>, do_json},
        {<<"application/x-json">>, do_json},
        {<<"application/javascript">>, do_c_comment},
        {<<"application/x-javascript">>, do_c_comment},
        {<<"text/css">>, do_c_comment},
        {<<"text/plain">>, do_text},
        {<<"application/atom+xml">>, [{<<"type">>, <<"entry">>}], do_empty},
        {<<"application/atom+xml">>, do_empty}
    ].

provide_extension(Context, Default) ->
    case map_extension(Context) of
        html ->
            [
                {<<"text/html">>, do_html},
                {<<"text/plain">>, do_text}
            ];
        json ->
            [
                {<<"application/json">>, do_json},
                {<<"application/x-json">>, do_json},
                {<<"text/plain">>, do_text}
            ];
        css ->
            [
                {<<"text/css">>, do_c_comment},
                {<<"text/plain">>, do_text}
            ];
        javascript ->
            [
                {<<"application/javascript">>, do_c_comment},
                {<<"application/x-javascript">>, do_c_comment},
                {<<"text/plain">>, do_text}
            ];
        text ->
            [
                {<<"text/plain">>, do_text}
            ];
        image ->
            provide_image();
        other ->
            Default
    end.

provide_image() ->
    [
        {<<"image/gif">>, do_image}
    ].

provide_html() ->
    [
        {<<"text/html">>, do_html},
        {<<"text/plain">>, do_text}
    ].

provide_text() ->
    [
        {<<"text/plain">>, do_text}
    ].

map_extension(Context) ->
    case z_string:to_lower(filename:extension(z_convert:to_binary(cowmachine_req:disp_path(Context)))) of
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

do_html(Context0) ->
    Context = set_headers(Context0),
    ContextQs = z_context:ensure_qs(Context),
    ErrorCode = error_code(Context),
    Vars = [
        {error_code, ErrorCode}
        | z_context:get_all(ContextQs)
    ],
    Vars1 = case bt_simplify(cowmachine_req:get_metadata(error_reason, Context)) of
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
    StatusTpl = <<"error.", (z_convert:to_binary(ErrorCode))/binary, ".tpl">>,
    Rendered = case z_module_indexer:find(template, StatusTpl, ContextQs) of
                    {ok, ModuleIndex} -> z_template:render(ModuleIndex, Vars1, ContextQs);
                    {error, enoent} -> z_template:render(<<"error.tpl">>, Vars1, ContextQs)
               end,
    z_context:output(Rendered, ContextQs).

error_code(Context) ->
    case z_context:get(http_status_code, Context) of
        StatusCode when is_integer(StatusCode) ->
            StatusCode;
        _ ->
            cowmachine_req:get_metadata(http_status_code, Context)
    end.

do_text(Context0) ->
    Context = set_headers(Context0),
    Text = httpd_util:reason_phrase(cowmachine_req:get_metadata(http_status_code, Context)),
    {Text, Context}.

do_json(Context0) ->
    Context = set_headers(Context0),
    StatusCode = cowmachine_req:get_metadata(http_status_code, Context),
    JSON = iolist_to_binary([
            <<"{ code: ">>, z_convert:to_binary(StatusCode),
            <<", status: \"">>, httpd_util:reason_phrase(StatusCode),
            <<"\" }">>
        ]),
    {JSON, Context}.

do_c_comment(Context0) ->
    Context = set_headers(Context0),
    StatusCode = cowmachine_req:get_metadata(http_status_code, Context),
    JSON = iolist_to_binary([
            <<"/* ">>, z_convert:to_binary(StatusCode),
            <<" ">>, httpd_util:reason_phrase(StatusCode),
            <<" */">>
        ]),
    {JSON, Context}.

do_empty(Context0) ->
    Context = set_headers(Context0),
    {<<>>, Context}.

do_image(Context0) ->
    Context = set_headers(Context0),
    {trans_gif(), Context}.

set_headers(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    z_context:set_nocache_headers(Context1).

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
bt_simplify({throw, {Reason, BT}}) when is_list(BT) ->
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
    atom_to_binary(A, 'utf8');
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
    SFun = case is_integer(ArityArgs) of
                true -> z_convert:to_list(Fun) ++ "/" ++ integer_to_list(ArityArgs);
                false -> z_convert:to_list(Fun) ++ "/" ++ integer_to_list(length(ArityArgs))
           end,
    case is_template(Module) of
        {true, TplName} ->
            [true, TplName, SFun, simplify_args(ArityArgs), loc(File,Line)];
        false ->
            [false, Module, SFun, simplify_args(ArityArgs), loc(File,Line)]
    end.

loc(undefined,undefined) -> {undefined, undefined};
loc(File,undefined) -> {z_convert:to_list(File), undefined};
loc(File,Line) -> {z_convert:to_list(File), integer_to_list(Line)}.

simplify_args(N) when is_integer(N) -> undefined;
simplify_args(L) ->
    As = [ simplify_arg(A) || A <- L ],
    iolist_to_binary(["[ ", z_utils:combine(", ", As), " ]"]).

simplify_arg(N) when is_integer(N) -> integer_to_list(N);
simplify_arg(A) when is_atom(A) -> atom_to_list(A);
simplify_arg(B) when is_binary(B) -> B;
simplify_arg([]) -> "[]";
simplify_arg({}) -> "{}";
simplify_arg(L) when is_list(L) -> "[...]";
simplify_arg({A,B}) when is_atom(A), is_atom(B) -> [${,atom_to_list(A),$,,atom_to_list(B),$}];
simplify_arg(#context{} = Context) ->
    [ "#context{site=", z_convert:to_binary(z_context:site(Context)), "}" ];
simplify_arg(T) when is_tuple(T) ->
    case is_atom(element(1, T)) of
        true -> [$#, atom_to_list(element(1,T)), "{}"];
        false -> io_lib:format("~p", [T])
    end;
simplify_arg(X) -> io_lib:format("~p", [X]).


is_template(Module) ->
    case template_compiler:is_template_module(Module) of
        true ->
            MF = Module:filename(),
            case binary:split(MF, <<"/lib/">>) of
                [_, Tpl] ->
                    case binary:split(Tpl, <<"/priv/templates/">>) of
                        [Mod, Template] -> {true, {Mod, Template}};
                        _ -> {true, {undefined, Tpl}}
                    end;
                _ ->
                    {true, {undefined, MF}}
            end;
        false ->
            false
    end.

