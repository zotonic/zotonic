%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Serve editor LSP sessions over stdio, forwarding template intelligence requests to the running Zotonic node.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_development_lsp).
-moduledoc(<<"
LSP 3.17 stdio session. Each editor owns its buffers; only bounded, read-only semantic requests cross the
authenticated Erlang node connection. Full document synchronization uses UTF-16 positions.
">>).

-export([run/1, new/0, handle/3, frame/1, header_length/1]).

-define(MAX_MESSAGE, 8388608).
-define(MAX_SOURCE, 4194304).

%% @doc Run a private stdio session; stdout contains protocol frames only.
-spec run(node()) -> no_return().
run(Node) ->
    ok = io:setopts(standard_io, [binary, {encoding, latin1}]),
    _ = logger:set_handler_config(default, config, #{type => standard_error}),
    Remote = fun(Kind, Args) ->
        case rpc:call(Node, z_development_lsp_backend, request, [Kind, Args], 10000) of
            {badrpc, _} -> {error, <<"Zotonic is unavailable; check the running node and restart the language server.">>};
            Result -> Result
        end
    end,
    Defaults = case os:getenv("ZOTONIC_LSP_PATH_MAPPINGS") of
        false -> [];
        Encoded -> unicode:characters_to_binary(Encoded)
    end,
    loop((new())#{inferred_mappings => Defaults}, Remote).


%% @doc Create an isolated document and lifecycle state for one editor.
-spec new() -> map().
new() ->
    #{
        phase => new,
        site => <<>>,
        documents => #{},
        path_mappings => [],
        inferred_mappings => [],
        hover_format => <<"plaintext">>
    }.

loop(State, Remote) ->
    case read_message() of
        eof ->
            halt(0);
        {error, Reason} ->
            io:format(standard_error, "Zotonic LSP: ~p~n", [Reason]),
            halt(1);
        {ok, Message} ->
            {Replies, Next} = handle(Message, State, Remote),
            lists:foreach(fun(R) -> file:write(standard_io, frame(R)) end, Replies),
            case Next of
                {exit, Code} -> halt(Code);
                _ -> loop(Next, Remote)
            end
    end.

%% @doc Bound headers and body before decoding JSON; Content-Length counts bytes.
read_message() ->
    case read_header(<<>>) of
        eof ->
            eof;
        {ok, Header} ->
            try header_length(Header) of
                Length ->
                    case file:read(standard_io, Length) of
                        {ok, Body} when is_binary(Body), byte_size(Body) =:= Length ->
                            try
                                {ok, z_json:decode(Body)}
                            catch
                                _:_ -> {ok, parse_error}
                            end;
                        _ ->
                            {error, truncated_body}
                    end
            catch
                _:_ -> {error, invalid_header}
            end;
        Error ->
            Error
    end.

read_header(Header) when byte_size(Header) >= 8192 -> {error, header_too_large};
read_header(Header) ->
    case file:read(standard_io, 1) of
        eof when Header =:= <<>> ->
            eof;
        eof ->
            {error, truncated_header};
        {ok, Char} when is_binary(Char) ->
            Next = <<Header/binary, Char/binary>>,
            case binary:match(Next, <<"\r\n\r\n">>) of
                nomatch -> read_header(Next);
                _ -> {ok, Next}
            end;
        _ ->
            {error, input_closed}
    end.

-spec header_length(binary()) -> pos_integer().
header_length(Header) ->
    Lengths = [
        string:trim(Value)
        || Line <- binary:split(Header, <<"\r\n">>, [global]),
           [Name, Value] <- [binary:split(Line, <<":">>)],
           string:lowercase(Name) =:= <<"content-length">>
    ],
    [Length] = Lengths,
    N = binary_to_integer(Length),
    true = N > 0 andalso N =< ?MAX_MESSAGE,
    N.

%% @doc Serialize one JSON-RPC message without encoding-dependent length errors.
-spec frame(map()) -> binary().
frame(Message) ->
    Body = z_json:encode(Message),
    iolist_to_binary([<<"Content-Length: ">>, integer_to_binary(byte_size(Body)), <<"\r\n\r\n">>, Body]).

%% @doc Validate protocol messages and keep malformed requests from corrupting session state.
-spec handle(term(), map(), fun((atom(), map()) -> term())) -> {list(), map() | {exit, 0 | 1}}.
handle(parse_error, State, _) ->
    {error_reply(#{<<"id">> => null}, -32700, <<"Invalid JSON.">>), State};
handle(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Message, State, Remote)
    when is_binary(Method) ->
    try
        dispatch(Method, maps:get(<<"params">>, Message, #{}), Message, State, Remote)
    catch
        throw:{lsp_error, Text} ->
            case maps:is_key(<<"id">>, Message) of
                true ->
                    {error_reply(Message, -32001, Text), State};
                false ->
                    {Notices, Next} = invalid_parameters(Message, State),
                    {[notification(<<"window/logMessage">>, #{type => 2, message => Text}) | Notices], Next}
            end;
        _:_ ->
            invalid_parameters(Message, State)
    end;
handle(#{<<"id">> := _} = Message, State, _) ->
    {error_reply(Message, -32600, <<"Invalid JSON-RPC request.">>), State};
handle(_, State, _) ->
    {[], State}.

%% @doc Invalid buffer updates invalidate the old text rather than serving stale positions.
invalid_parameters(#{ <<"id">> := _ } = Message, State) ->
    {error_reply(Message, -32602, <<"Invalid request parameters.">>), State};
invalid_parameters(#{ <<"method">> := Method, <<"params">> := #{ <<"textDocument">> := #{ <<"uri">> := Uri } } }, State)
    when Method =:= <<"textDocument/didOpen">>; Method =:= <<"textDocument/didChange">> ->
    Warning = notification(
        <<"window/logMessage">>,
        #{
            type => 2,
            message => <<"Zotonic rejected a buffer update (invalid URI, encoding, version or size limit). Close and reopen the document after correcting it.">>
        }),
    {[
        publish(Uri, [], undefined),
        Warning
     ],
     State#{
        documents => maps:remove(Uri, maps:get(documents, State))
    }};
invalid_parameters(_, State) ->
    {[], State}.

dispatch(<<"exit">>, _, _, #{phase := shutdown}, _) -> {[], {exit, 0}};
dispatch(<<"exit">>, _, _, _, _) -> {[], {exit, 1}};
dispatch(<<"initialize">>, Params, Message, #{phase := new} = State, _) ->
    true = maps:is_key(<<"id">>, Message),
    Options = case maps:get(<<"initializationOptions">>, Params, #{}) of null -> #{}; O -> O end,
    Site = site_setting(Options),
    Mappings = case maps:get(<<"pathMappings">>, Options, []) of
        [] -> prepare_inferred_mappings(maps:get(inferred_mappings, State));
        ExplicitMappings -> z_development_lsp_paths:prepare(ExplicitMappings)
    end,
    Result = #{
        capabilities => #{
            positionEncoding => <<"utf-16">>,
            textDocumentSync => #{
                openClose => true,
                change => 1
            },
            definitionProvider => true,
            hoverProvider => true,
            completionProvider => #{
                triggerCharacters => [<<"\"">>, <<"/">>, <<"|">>, <<".">>]
            }
        },
        serverInfo => #{
            name => <<"Zotonic">>,
            version => <<"0.1.0">>
        }
    },
    {
        reply(Message, Result),
        State#{
            phase => running,
            site => Site,
            path_mappings => Mappings,
            hover_format => hover_format(Params),
            hover_tables => hover_tables(Params)
        }
    };
dispatch(_, _, Message, #{phase := new} = State, _) ->
    {error_reply(Message, -32002, <<"Server not initialized.">>), State};
dispatch(_, _, Message, #{phase := shutdown} = State, _) ->
    {error_reply(Message, -32600, <<"Server has shut down.">>), State};
dispatch(<<"shutdown">>, _, Message, State, _) ->
    {reply(Message, null), State#{phase => shutdown, documents => #{}}};
dispatch(<<"initialized">>, _, _, State, _) -> {[], State};
dispatch(<<"$/cancelRequest">>, _, _, State, _) -> {[], State};
dispatch(<<"textDocument/didOpen">>, #{<<"textDocument">> := Doc}, _, State, Remote) ->
    #{<<"uri">> := Uri, <<"text">> := Text, <<"version">> := Version} = Doc,
    put_document(Uri, Text, Version, State, Remote);
dispatch(<<"textDocument/didChange">>, #{<<"textDocument">> := Doc,
        <<"contentChanges">> := [_ | _] = Changes}, _, State, Remote) ->
    Text = lists:foldl(fun full_change/2, <<>>, Changes),
    #{<<"uri">> := Uri, <<"version">> := Version} = Doc,
    #{version := Previous} = maps:get(Uri, maps:get(documents, State)),
    case Version > Previous of
        true -> put_document(Uri, Text, Version, State, Remote);
        false -> {[], State}
    end;
dispatch(<<"textDocument/didClose">>, #{<<"textDocument">> := #{<<"uri">> := Uri}}, _, State, _) ->
    {[publish(Uri, [], undefined)], State#{documents => maps:remove(Uri, maps:get(documents, State))}};
dispatch(<<"workspace/didChangeConfiguration">>, #{<<"settings">> := Settings}, _, State, _) ->
    {[], State#{site => site_setting(maps:get(<<"zotonic">>, Settings, Settings))}};
dispatch(<<"zotonic/sites">>, _, Message, State, Remote) ->
    {remote_reply(Message, Remote(sites, #{})), State};
dispatch(Method, #{<<"textDocument">> := #{<<"uri">> := Uri}, <<"position">> := Position}, Message, State, Remote)
    when Method =:= <<"textDocument/definition">>; Method =:= <<"textDocument/hover">>;
         Method =:= <<"textDocument/completion">> ->
    #{source := Source, file := File} = maps:get(Uri, maps:get(documents, State)),
    _ = z_development_lsp_text:prefix(Source, Position),
    Kind = case Method of
        <<"textDocument/definition">> -> definition;
        <<"textDocument/hover">> -> hover;
        <<"textDocument/completion">> -> completion
    end,
    Result = Remote(Kind, #{source => Source, file => File, position => Position, site => maps:get(site, State), hover_format => maps:get(hover_format, State)}),
    ClientResult = case Kind of
        definition -> z_development_lsp_paths:definitions(Result, maps:get(path_mappings, State));
        hover -> format_hover(Result, maps:get(hover_tables, State, markdown));
        _ -> Result
    end,
    {remote_reply(Message, ClientResult), State};
dispatch(_, _, Message, State, _) ->
    {error_reply(Message, -32601, <<"Method not supported.">>), State}.

%% @doc Sublime's minihtml cannot render tables; other clients retain Markdown tables.
hover_tables(Params) ->
    case maps:get(<<"clientInfo">>, Params, #{}) of
        #{<<"name">> := Name} when is_binary(Name) ->
            case binary:match(string:lowercase(Name), <<"sublime">>) of
                nomatch -> markdown;
                _ -> lists
            end;
        _ ->
            markdown
    end.

format_hover(#{contents := #{kind := <<"markdown">>, value := Text} = Contents} = Hover, lists) ->
    Hover#{contents => Contents#{value => z_development_lsp_tables:to_lists(Text)}};
format_hover(Result, _) -> Result.

%% @doc Honor the client's preferred supported hover format; older clients get plain text.
hover_format(Params) ->
    Capabilities = maps:get(<<"capabilities">>, Params, #{}),
    TextDocument = maps:get(<<"textDocument">>, Capabilities, #{}),
    Hover = maps:get(<<"hover">>, TextDocument, #{}),
    Formats = maps:get(<<"contentFormat">>, Hover, []),
    case [F || F <- Formats, F =:= <<"markdown">> orelse F =:= <<"plaintext">>] of
        [Format | _] -> Format;
        [] -> <<"plaintext">>
    end.

%% @doc Decode and validate inference only when explicit editor mappings are absent.
prepare_inferred_mappings(Encoded) when is_binary(Encoded) ->
    z_development_lsp_paths:prepare(z_json:decode(Encoded));
prepare_inferred_mappings(Mappings) ->
    z_development_lsp_paths:prepare(Mappings).

site_setting(Settings) ->
    Site = maps:get(<<"site">>, Settings, <<>>),
    true = is_binary(Site) andalso byte_size(Site) < 256,
    Site.

%% @doc Validate successive full snapshots and retain the last one.
full_change(#{<<"text">> := Text} = Change, _) ->
    false = maps:is_key(<<"range">>, Change),
    true = is_binary(Text) andalso byte_size(Text) =< ?MAX_SOURCE,
    Text = unicode:characters_to_binary(Text),
    Text.

%% @doc Store full buffers with monotonic versions and bounded aggregate memory.
put_document(Uri, Text, Version, #{documents := Documents} = State, Remote) ->
    true = is_binary(Text) andalso byte_size(Text) =< ?MAX_SOURCE andalso is_integer(Version),
    Text = unicode:characters_to_binary(Text),
    File = z_development_lsp_paths:to_server(z_development_lsp_text:path(Uri), maps:get(path_mappings, State)),
    <<".tpl">> = filename:extension(File),
    Updated = Documents#{Uri => #{source => Text, file => File, version => Version}},
    true = map_size(Updated) =< 200,
    true = lists:sum([byte_size(S) || #{source := S} <- maps:values(Updated)]) =< 67108864,
    case Remote(diagnostics, #{source => Text}) of
        {error, Error} ->
            {[notification(<<"window/logMessage">>, #{type => 1, message => Error})], State#{documents => Updated}};
        Diagnostics ->
            {[publish(Uri, Diagnostics, Version)], State#{documents => Updated}}
    end.

publish(Uri, Diagnostics, Version) ->
    Params = #{uri => Uri, diagnostics => Diagnostics},
    notification(<<"textDocument/publishDiagnostics">>, case Version of
        undefined -> Params;
        _ -> Params#{version => Version}
    end).

notification(Method, Params) ->
    #{
        jsonrpc => <<"2.0">>,
        method => Method,
        params => Params
    }.

reply(#{<<"id">> := Id}, Result) ->
    [
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            result => Result
        }
    ];
reply(_, _) ->
    [].

remote_reply(Message, {error, Text}) ->
    error_reply(Message, -32001, Text);
remote_reply(Message, Result) ->
    reply(Message, Result).

error_reply(#{<<"id">> := Id}, Code, Text) ->
    [
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            error => #{
                code => Code,
                message => Text
            }
        }
    ];
error_reply(_, _, _) ->
    [].
