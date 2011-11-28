%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Webmachine HTTP Request Abstraction.

-module(webmachine_request).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-include("../../../include/zotonic_release.hrl").

-export([get_peer/1]). % used in initialization

% actual interface for resource functions
-export([
     socket/1,
     method/1,
     version/1,
     disp_path/1,
     path/1,
     raw_path/1,
     get_req_header/2,
     req_headers/1,
     req_body/2,
     stream_req_body/2,
     headers/1,
     resp_headers/1,
     out_headers/1,
     get_out_header/2,
     has_out_header/2,
     peer/1,
     get_header_value/2,
     add_response_header/3,
     add_response_headers/2,
     remove_response_header/2,
     merge_response_headers/2,
     append_to_response_body/2,
     send_response/1,
     send_response/2,
     response_code/1,
     set_response_code/2,
     set_resp_body/2,
     response_body/1,
     has_response_body/1,
     has_resp_body/1,
     do_redirect/1,
     resp_redirect/1,
     set_metadata/3,
     get_metadata/2,
     get_path_info/1,
     get_path_info/2,
     load_dispatch_data/7,
     get_path_tokens/1,
     get_app_root/1,
     parse_cookie/1,
     get_cookie_value/2,
     parse_qs/1,
     get_qs_value/2,
     get_qs_value/3,
     range/1,
     log_data/1
     ]).

-include("webmachine_logger.hrl").
-include_lib("include/wm_reqdata.hrl").

-define(WMVSN, "1.7.3 (compat)").
-define(QUIP, "humming along").
-define(IDLE_TIMEOUT, infinity).

get_peer(ReqData) ->
    case ReqData#wm_reqdata.peer of
    	undefined ->
            Socket = ReqData#wm_reqdata.socket,
            Peer = peer_from_peername(mochiweb_socket:peername(Socket), ReqData),
            NewReqData = ReqData#wm_reqdata{peer=Peer},
            {Peer, NewReqData};
    	_ ->
    	    {ReqData#wm_reqdata.peer, ReqData}
    end.

peer_from_peername({ok, {Addr={10, _, _, _}, _Port}}, ReqData) ->  
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {Addr={172, Second, _, _}, _Port}}, ReqData) when (Second > 15) andalso (Second < 32) ->
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {Addr={192, 168, _, _}, _Port}}, ReqData) ->
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {{127, 0, 0, 1}, _Port}}, ReqData) ->
    x_peername("127.0.0.1", ReqData);
peer_from_peername({ok, {Addr, _Port}}, _ReqData) ->
    inet_parse:ntoa(Addr).

x_peername(Default, ReqData) ->
    case get_header_value("x-forwarded-for", ReqData) of
    undefined ->
        Default;
    Hosts ->
        string:strip(lists:last(string:tokens(Hosts, ",")))
    end.


get_header_value(K, ReqData) ->
    wrq:get_req_header(K, ReqData).

get_outheader_value(K, ReqData) ->
    mochiweb_headers:get_value(K, wrq:resp_headers(ReqData)).

send(undefined, _Data) ->
    ok;
send(Socket, Data) ->
    case mochiweb_socket:send(Socket, iolist_to_binary(Data)) of
	ok -> ok;
	{error,closed} -> ok;
	_ -> exit(normal)
    end.

send_stream_body(Socket, IsChunked, X) -> 
    send_stream_body(Socket, IsChunked, X, 0).

send_stream_body(Socket, IsChunked, {<<>>, done}, SoFar) ->
    send_chunk(Socket, IsChunked, <<>>),
    SoFar;
send_stream_body(Socket, IsChunked, {Data, done}, SoFar) ->
    Size = send_chunk(Socket, IsChunked, Data),
    send_chunk(Socket, IsChunked, <<>>),
    Size + SoFar;
send_stream_body(Socket, IsChunked, {<<>>, Next}, SoFar) ->
    send_stream_body(Socket, IsChunked, Next(), SoFar);
send_stream_body(Socket, IsChunked, {[], Next}, SoFar) ->
    send_stream_body(Socket, IsChunked, Next(), SoFar);
send_stream_body(Socket, IsChunked, {Data, Next}, SoFar) ->
    Size = send_chunk(Socket, IsChunked, Data),
    send_stream_body(Socket, IsChunked, Next(), Size + SoFar).


%% @todo Remove usage of the put/get to get the number of bytes written
%%       Use a separate process to accumulate these counts
send_writer_body(Socket, IsChunked, {Encoder, Charsetter, BodyFun}) ->
    put(bytes_written, 0),
    Writer = fun(Data) ->
        Size = send_chunk(Socket, IsChunked, Encoder(Charsetter(Data))),
        put(bytes_written, get(bytes_written) + Size),
        Size
    end,
    BodyFun(Writer),
    send_chunk(Socket, IsChunked, <<>>),
    get(bytes_written).


%% @todo Distinguish between HTTP/1.0 and 1.1
%%       HTTP/1.0 should not add the size (and transfer-encoding: chunked)
send_chunk(Socket, true, Data) ->
    Size = iolist_size(Data),
    send(Socket, mochihex:to_hex(Size)),
    send(Socket, <<"\r\n">>),
    send(Socket, Data),
    send(Socket, <<"\r\n">>),
    Size;
send_chunk(_Socket, false, <<>>) ->
    0;
send_chunk(Socket, false, Data) ->
    Size = iolist_size(Data),
    send(Socket, Data),
    Size.


send_response(ReqData) ->
	{Reply, RD1} = case ReqData#wm_reqdata.response_code of
        200 -> send_ok_response(ReqData);
        Code -> send_response(Code, ReqData)
    end,
    LogData = RD1#wm_reqdata.log_data,
    NewLogData = LogData#wm_log_data{finish_time=now()},
    {Reply, RD1#wm_reqdata{log_data=NewLogData}}.

send_ok_response(ReqData) ->
    {Range, RangeRD} = get_range(ReqData),
    case Range of
	X when X =:= undefined; X =:= fail ->
	    send_response(200, RangeRD);
	Ranges ->
	    {PartList, Size} = range_parts(RangeRD, Ranges),
	    case PartList of
		[] -> %% no valid ranges
		    %% could be 416, for now we'll just return 200
		    send_response(200, RangeRD);
		PartList ->
		    {RangeHeaders, RangeBody} = parts_to_body(PartList, Size, RangeRD),
            RespHdrsRD = wrq:set_resp_headers([{"Accept-Ranges", "bytes"} | RangeHeaders], RangeRD),
            RespBodyRD = wrq:set_resp_body(RangeBody, RespHdrsRD),
		    send_response(206, RespBodyRD)
	    end
    end.

send_response(Code, ReqData) ->
    Body0 = wrq:resp_body(ReqData),
    {Body, Transfer, Length} = case Body0 of
        {stream, StreamBody} -> {{stream, StreamBody}, chunked, undefined};
        {writer, WriteBody} -> {{writer, WriteBody}, chunked, undefined};
        {stream, Size, Fun} -> {{stream, Fun(0, Size-1)}, chunked, undefined};
        _ -> {Body0, undefined, iolist_size([Body0])}
    end,
    send(ReqData#wm_reqdata.socket,
        [make_version(wrq:version(ReqData)),
         make_code(Code), <<"\r\n">> | 
         make_headers(Code, Transfer, Length, ReqData)]),
    FinalLength = case wrq:method(ReqData) of 
	'HEAD' ->
	    Length;
	_ -> 
        case Body of
            {stream, Body2} ->
                send_stream_body(ReqData#wm_reqdata.socket, is_chunked_transfer(wrq:version(ReqData), Transfer), Body2);
            {writer, Body2} ->
                send_writer_body(ReqData#wm_reqdata.socket, is_chunked_transfer(wrq:version(ReqData), Transfer), Body2);
            _ ->
                send(ReqData#wm_reqdata.socket, Body),
                Length
        end
    end,
    InitLogData = ReqData#wm_reqdata.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=Code,response_length=FinalLength},
    ReqData1 = wrq:set_response_code(Code, ReqData),
    {ok, ReqData1#wm_reqdata{log_data=FinalLogData}}.
    


%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length(ReqData) ->
    case get_header_value("transfer-encoding", ReqData) of
        undefined ->
            case get_header_value("content-length", ReqData) of
                undefined -> undefined;
                Length -> list_to_integer(Length)
            end;
        "chunked" -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length
do_recv_body(ReqData) ->
    MRB = ReqData#wm_reqdata.max_recv_body,
    read_whole_stream(recv_stream_body(ReqData, MRB), [], MRB, 0).

read_whole_stream({Hunk,_}, _, MaxRecvBody, SizeAcc)
  when SizeAcc + byte_size(Hunk) > MaxRecvBody -> 
    {error, req_body_too_large};
read_whole_stream({Hunk,Next}, Acc0, MaxRecvBody, SizeAcc) ->
    HunkSize = byte_size(Hunk),
    if SizeAcc + HunkSize > MaxRecvBody -> 
            {error, req_body_too_large};
       true ->
            Acc = [Hunk|Acc0],
            case Next of
                done -> iolist_to_binary(lists:reverse(Acc));
                _ -> read_whole_stream(Next(), Acc, MaxRecvBody, SizeAcc + HunkSize)
            end
    end.

recv_stream_body(ReqData, MaxHunkSize) ->
    put(mochiweb_request_recv, true),
    case get_header_value("expect", ReqData) of
	"100-continue" ->
	    send(ReqData#wm_reqdata.socket, [make_version(wrq:version(ReqData)), make_code(100), <<"\r\n\r\n">>]);
	_Else ->
	    ok
    end,
    case body_length(ReqData) of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined -> {<<>>, done};
        0 -> {<<>>, done};
        chunked -> recv_chunked_body(ReqData#wm_reqdata.socket, MaxHunkSize);
        Length -> recv_unchunked_body(ReqData#wm_reqdata.socket, MaxHunkSize, Length)
    end.

recv_unchunked_body(Socket, MaxHunk, DataLeft) ->
    case MaxHunk >= DataLeft of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,DataLeft,?IDLE_TIMEOUT),
            {Data1, done};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_unchunked_body(Socket, MaxHunk, DataLeft-MaxHunk)
             end}
    end.
    
recv_chunked_body(Socket, MaxHunk) ->
    case read_chunk_length(Socket) of
        0 -> {<<>>, done};
        ChunkLength -> recv_chunked_body(Socket,MaxHunk,ChunkLength)
    end.
recv_chunked_body(Socket, MaxHunk, LeftInChunk) ->
    case MaxHunk >= LeftInChunk of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,LeftInChunk,?IDLE_TIMEOUT),
            {Data1,
             fun() -> recv_chunked_body(Socket, MaxHunk)
             end};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_chunked_body(Socket, MaxHunk, LeftInChunk-MaxHunk)
             end}
    end.

read_chunk_length(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            case Hex of
                [] -> 0;
                _ -> erlang:list_to_integer(Hex, 16)
            end;
        _ ->
            exit(normal)
    end.

get_range(ReqData) ->
    case get_header_value("range", ReqData) of
	undefined ->
	    {undefined, ReqData#wm_reqdata{range=undefined}};
	RawRange ->
	    Range = parse_range_request(RawRange),
	    {Range, ReqData#wm_reqdata{range=Range}}
    end.


range_parts(_RD=#wm_reqdata{resp_body={file, IoDevice}}, Ranges) ->
    Size = iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(RD=#wm_reqdata{resp_body={stream, {Hunk,Next}}}, Ranges) ->
    % for now, streamed bodies are read in full for range requests
    MRB = RD#wm_reqdata.max_recv_body,
    range_parts(read_whole_stream({Hunk,Next}, [], MRB, 0), Ranges);

range_parts(_RD=#wm_reqdata{resp_body={stream, Size, StreamFun}}, Ranges) ->
    SkipLengths = [ range_skip_length(R, Size) || R <- Ranges],
    {[ {Skip, Skip+Length-1, StreamFun} || {Skip, Length} <- SkipLengths ],
     Size};

range_parts(_RD=#wm_reqdata{resp_body=Body0}, Ranges) ->
    range_parts(Body0, Ranges);

range_parts(Body0, Ranges) when is_binary(Body0); is_list(Body0) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

parts_to_body([{Start, End, Body0}], Size, ReqData) ->
    %% return body for a range reponse with a single body
    ContentType = 
	case get_outheader_value("content-type", ReqData) of
	    undefined -> "text/html";
	    CT -> CT
	end,
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    make_io(Start), "-", make_io(End),
                    "/", make_io(Size)]}],
    Body = if is_function(Body0) ->
                   {stream, Body0(Start, End)};
              true ->
                   Body0
           end,
    {HeaderList, Body};
parts_to_body(BodyList, Size, ReqData) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    ContentType = 
	case get_outheader_value("content-type", ReqData) of
	    undefined -> "text/html";
	    CT -> CT
	end,
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = case hd(BodyList) of
                        {_, _, Fun} when is_function(Fun) ->
                            stream_multipart_body(BodyList, ContentType,
                                                  Boundary, Size);
                        _ ->
                            multipart_body(BodyList, ContentType,
                                           Boundary, Size)
                    end,
    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    end_boundary(Boundary);
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    [part_preamble(Boundary, ContentType, Start, End, Size),
     Body, <<"\r\n">>
      | multipart_body(BodyList, ContentType, Boundary, Size)].

boundary(B)     -> [<<"--">>, B, <<"\r\n">>].
end_boundary(B) -> [<<"--">>, B, <<"--\r\n">>].

part_preamble(Boundary, CType, Start, End, Size) ->
    [boundary(Boundary),
     <<"Content-Type: ">>, CType, <<"\r\n">>,
     <<"Content-Range: bytes ">>,
     mochiweb_util:make_io(Start), <<"-">>, mochiweb_util:make_io(End),
     <<"/">>, mochiweb_util:make_io(Size),
     <<"\r\n\r\n">>].

stream_multipart_body(BodyList, ContentType, Boundary, Size) ->
    Helper = stream_multipart_body_helper(
               BodyList, ContentType, Boundary, Size),
    %% executing Helper() here is an optimization;
    %% it's just as valid to say {<<>>, Helper}
    {stream, Helper()}.

stream_multipart_body_helper([], _CType, Boundary, _Size) ->
    fun() -> {end_boundary(Boundary), done} end;
stream_multipart_body_helper([{Start, End, Fun}|Rest],
                             CType, Boundary, Size) ->
    fun() ->
            {part_preamble(Boundary, CType, Start, End, Size),
             stream_multipart_part_helper(
               fun() -> Fun(Start, End) end,
               Rest, CType, Boundary, Size)}
    end.

stream_multipart_part_helper(Fun, Rest, CType, Boundary, Size) ->
    fun() ->
            case Fun() of
                {Data, done} ->
                    %% when this part is done, start the next part
                    {[Data, <<"\r\n">>],
                     stream_multipart_body_helper(
                       Rest, CType, Boundary, Size)};
                {Data, Next} ->
                    %% this subpart has more data coming
                    {Data, stream_multipart_part_helper(
                             Next, Rest, CType, Boundary, Size)}
            end
    end.

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

make_headers(Code, Transfer, Length, RD) ->
    Hdrs0 = case Code of
        304 ->
            mochiweb_headers:make(wrq:resp_headers(RD));
        _ -> 
            LengthHeaders = case Length of
                undefined ->
                    mochiweb_headers:make(wrq:resp_headers(RD));
                _Length -> 
                    mochiweb_headers:enter(
                      "Content-Length",integer_to_list(Length),
                      mochiweb_headers:make(wrq:resp_headers(RD)))
            end,
            
            case is_chunked_transfer(wrq:version(RD), Transfer) of
                true ->
                    mochiweb_headers:enter(
                      "Transfer-Encoding","chunked",
                      LengthHeaders);
                false ->
                    LengthHeaders
            end
    end,
    ServerHeader = "MochiWeb/1.1 WebMachine/" ++ ?WMVSN ++ " Zotonic/" ++ ?ZOTONIC_VERSION,
    WithSrv = mochiweb_headers:enter("Server", ServerHeader, Hdrs0),
    Hdrs = case mochiweb_headers:get_value("date", WithSrv) of
	undefined ->
            mochiweb_headers:enter("Date", httpd_util:rfc1123_date(), WithSrv);
	_ ->
	    WithSrv
    end,
    F = fun({K, V}, Acc) ->
		[make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
	end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Hdrs)).


is_chunked_transfer(HttpVersion, chunked) when HttpVersion > {1,0} -> true;
is_chunked_transfer(_, _) -> false.


socket(ReqData) -> ReqData#wm_reqdata.socket.

method(ReqData) -> wrq:method(ReqData).

version(ReqData) -> wrq:version(ReqData).

disp_path(ReqData) -> wrq:disp_path(ReqData).

path(ReqData) -> wrq:path(ReqData).

raw_path(ReqData) -> wrq:raw_path(ReqData).

req_headers(ReqData) -> wrq:req_headers(ReqData).
headers(ReqData) -> req_headers(ReqData).

req_body(MaxRecvBody, ReqData) ->
    case ReqData#wm_reqdata.bodyfetch of
        stream ->
            {stream_conflict, ReqData};
        _ ->
            NewReqData = ReqData#wm_reqdata{max_recv_body=MaxRecvBody},
            case NewReqData#wm_reqdata.req_body of
                not_fetched_yet ->
                    NewBody = do_recv_body(NewReqData),
                    NewRD = NewReqData#wm_reqdata{req_body=NewBody,bodyfetch=standard},
                    {NewBody, NewRD};
                X ->
                    {X, ReqData#wm_reqdata{bodyfetch=standard}}
            end
    end.

stream_req_body(MaxHunk, ReqData) -> 
    case ReqData#wm_reqdata.bodyfetch of
        standard ->
            {stream_conflict, ReqData};
        _ ->
            {recv_stream_body(ReqData, MaxHunk), ReqData#wm_reqdata{bodyfetch=stream}}
    end.

resp_headers(ReqData) -> wrq:resp_headers(ReqData).
out_headers(ReqData) -> resp_headers(ReqData).

get_resp_header(HdrName, ReqData) -> mochiweb_headers:get_value(HdrName, wrq:resp_headers(ReqData)).
get_out_header(HeaderName, ReqData) -> get_resp_header(HeaderName, ReqData).

has_resp_header(HeaderName, ReqData) ->
    case get_out_header(HeaderName, ReqData) of
        undefined -> false;
        _ -> true
    end.
has_out_header(HeaderName, ReqData) -> has_resp_header(HeaderName, ReqData).

has_resp_body(ReqData) ->
    case wrq:resp_body(ReqData) of
        undefined -> false;
        <<>> -> false;
        [] -> false;
        _ -> true
    end.
has_response_body(ReqData) -> 
    has_resp_body(ReqData).

response_code(ReqData) -> wrq:response_code(ReqData).
set_response_code(Code, ReqData) -> wrq:set_response_code(Code, ReqData).

peer(ReqData) -> get_peer(ReqData).

range(ReqData) -> get_range(ReqData).

req_cookie(ReqData) -> wrq:req_cookie(ReqData).
parse_cookie(ReqData) -> req_cookie(ReqData).
get_cookie_value(Key, ReqData) -> proplists:get_value(Key, req_cookie(ReqData)).

req_qs(ReqData) -> wrq:req_qs(ReqData).
parse_qs(ReqData) -> req_qs(ReqData).
get_qs_value(Key, ReqData) -> proplists:get_value(Key, req_qs(ReqData)).
get_qs_value(Key, Default, ReqData) -> proplists:get_value(Key, req_qs(ReqData), Default).

set_resp_body(Body, ReqData) -> wrq:set_resp_body(Body, ReqData).

resp_body(ReqData) -> wrq:resp_body(ReqData).
response_body(ReqData) -> resp_body(ReqData).

get_req_header(K, ReqData) -> wrq:get_req_header(K, ReqData).

set_resp_header(K, V, ReqData) -> wrq:set_resp_header(K, V, ReqData).
add_response_header(K, V, ReqData) -> set_resp_header(K, V, ReqData).

set_resp_headers(Hdrs, ReqData) ->  wrq:set_resp_headers(Hdrs, ReqData).
add_response_headers(Hdrs, ReqData) -> set_resp_headers(Hdrs, ReqData).

remove_resp_header(K, ReqData) -> wrq:remove_resp_header(K, ReqData).
remove_response_header(K, ReqData) -> remove_resp_header(K, ReqData).

merge_resp_headers(Hdrs, ReqData) -> wrq:merge_resp_headers(Hdrs, ReqData).
merge_response_headers(Hdrs, ReqData) -> merge_resp_headers(Hdrs, ReqData).

append_to_response_body(Data, ReqData) -> wrq:append_to_response_body(Data, ReqData).

do_redirect(ReqData) -> wrq:do_redirect(true, ReqData).

resp_redirect(ReqData) -> wrq:resp_redirect(ReqData).

get_metadata(Key, ReqData) ->
    case dict:find(Key, ReqData#wm_reqdata.metadata) of
		{ok, Value} -> Value;
		error -> undefined
	end.

set_metadata(Key, Value, ReqData) ->
    NewDict = dict:store(Key, Value, ReqData#wm_reqdata.metadata),
    {ok, ReqData#wm_reqdata{metadata=NewDict}}.

get_path_info(ReqData) -> dict:to_list(wrq:path_info(ReqData)).

get_path_info(Key, ReqData) -> wrq:path_info(Key, ReqData).

path_tokens(ReqData) -> wrq:path_tokens(ReqData).
get_path_tokens(ReqData) -> path_tokens(ReqData).

app_root(ReqData) -> wrq:app_root(ReqData).
get_app_root(ReqData) -> app_root(ReqData).

load_dispatch_data(Bindings, HostTokens, Port, PathTokens, AppRoot, DispPath, ReqData) ->
    PathInfo = dict:from_list(Bindings),
    RD1 = wrq:load_dispatch_data(
                        PathInfo,HostTokens,Port,PathTokens,AppRoot,
                        DispPath,ReqData),
    {ok, RD1}.


log_data(ReqData) ->
    ReqData#wm_reqdata.log_data.
