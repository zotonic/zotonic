%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012  Marc Worrell
%%
%% @doc Error handler for webmachine HTTP errors. The result varies depending on the content type being served.
%% @todo Mail the error to the webadmin

%% Copyright 2009-2012 Marc Worrell, Arjan Scherpenisse
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

-compile([{parse_transform, lager_transform}]).

-export([render_error/3]).

-include_lib("zotonic.hrl").


render_error(Code = 500, ReqData, Reason) ->
    lager:error("webmachine error: path=~p: ~p", [wrq:path(ReqData), Reason]),
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("~p", [Reason]))),
    Type = webmachine_request:get_metadata('content-type', ReqData),
    error_handler(Type, ReqData, Code, ErrorDump, Reason);

render_error(Code = 404, ReqData, _Reason) ->
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("Resource not found: ~p", [wrq:raw_path(ReqData)]))),
    Type = webmachine_request:get_metadata('content-type', ReqData),
    error_handler(Type, ReqData, Code, ErrorDump, undefined);

render_error(Code, ReqData, _Reason) ->
    Type = webmachine_request:get_metadata('content-type', ReqData),
    error_handler(Type, ReqData, Code, undefined, undefined).


error_handler("application/json", ReqData, Code, ErrorDump, _Reason) ->
    RD1 = wrq:set_resp_header("Content-Type", "application/json; charset=utf-8", ReqData),
    RD2 = wrq:set_resp_header("Content-Encoding", "identity", RD1),
    JS = {struct, [{error_code, Code}, {error_dump, ErrorDump}]},
    {mochijson:encode(JS), RD2};

% Check the extension of the path to see what the resource could have been.
error_handler(_, ReqData, Code, ErrorDump, Reason) ->
    case z_media_identify:guess_mime(wrq:raw_path(ReqData)) of
        "application/octet-stream" -> show_template(ReqData, Code, ErrorDump, Reason);
        "application/xhtml+xml" -> show_template(ReqData, Code, ErrorDump, Reason);
        "application/" ++ _ -> {<<>>, ReqData};
        "audio/" ++ _ -> {<<>>, ReqData};
        "video/" ++ _ -> {<<>>, ReqData};
        "image/" ++ _ -> {<<>>, ReqData};
        "text/css" -> {<<>>, ReqData};
        _ -> show_template(ReqData, Code, ErrorDump, Reason)
    end.


show_template(ReqData, Code, ErrorDump, Reason) ->
    RD1 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", ReqData),
    RD2 = wrq:set_resp_header("Content-Encoding", "identity", RD1),
    RD3 = wrq:set_resp_header("X-Robots", "noindex,nofollow", RD2),
    Host = webmachine_request:get_metadata('zotonic_host', RD3),
    try 
        Context = z_context:new(Host),
        Vars = case bt_simplify(Reason) of
                    {reason, ErlangError, Tab} ->
                        [
                            {error_code, Code},
                            {error_dump, ErrorDump},
                            {error_erlang, ErlangError},
                            {error_table, Tab}
                        ];
                    {raw, X} ->
                        [
                            {error_code, Code}, 
                            {error_dump, X}
                        ]
               end,
        Html = z_template:render("error.tpl", Vars, Context),
        {Output, _} = z_context:output(Html, Context),
        {Output, RD3}
    catch
        _:_Reason -> {<<>>,RD3}
    end.


bt_simplify({_E1, {_E2, Reason, BT}}) when is_list(BT) ->
    {reason, Reason, bt_table(BT)};
bt_simplify({_E, {Reason, BT}}) when is_list(BT) ->
    {reason, Reason, bt_table(BT)};
bt_simplify(X) ->
    {raw, X}.

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
