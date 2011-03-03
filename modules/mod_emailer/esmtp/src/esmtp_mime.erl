%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Email MIME encoding library.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_mime).

-include_lib("../include/esmtp_mime.hrl").

%% API
-export([encode/1, send/5,
         msg/0, msg/3, msg/4,
         from/1, to/1,
         create_text_part/1,
         create_html_part/1,
         create_multipart/0,
         add_part/2,
         add_header/2,
         create_attachment/2,
         create_attachment/3,
         create_attachment/4,
         set_multipart_type/2
]).

-export([test_msg/0,
         send_test/4,
         test/0,
         double_dot/1
]).

%%====================================================================
%% API
%%====================================================================

msg(To, From, Subject) ->
    Msg = msg(),
    Msg1 = add_header(Msg, {"To", To}),
    Msg2 = add_header(Msg1, {"Subject", Subject}),
    add_header(Msg2, {"From", From}).

msg(To, From, Subject, Body) ->
    Msg = msg(To, From, Subject),
    add_part(Msg, create_text_part(Body)).

msg() ->
    #mime_multipart{boundary=invent_mime_boundary(),
                    headers=[{"Date", httpd_util:rfc1123_date()}, {"MIME-Version", "1.0"}]}.

set_multipart_type(M = #mime_multipart{}, T) ->
    M#mime_multipart{multipart_type=T}.


encode(Msg) ->
    encode_headers(headers(Msg)) ++ "\r\n\r\n" ++
        encode_parts(Msg) ++
        "--" ++ Msg#mime_multipart.boundary ++ "--\r\n".

to(#mime_multipart{headers=H}) ->
    proplists:get_value("To", H, undefined).

from(#mime_multipart{headers=H}) ->
    proplists:get_value("From", H, undefined).


add_part(Msg = #mime_multipart{parts=Parts}, Part) ->
    Msg#mime_multipart{parts=Parts++[Part]}.

create_text_part(Text) ->
    TextEncoded = z_quoted_printable:encode(Text),
    #mime_part{data=TextEncoded, encoding={"quoted-printable", "text/plain", "utf-8"}}.

create_html_part(Html) ->
    HtmlEncoded = z_quoted_printable:encode(Html),
    #mime_part{data=HtmlEncoded, encoding={"quoted-printable", "text/html", "utf-8"}}.

create_multipart() ->
    #mime_multipart{boundary=invent_mime_boundary()}.


add_header(Msg = #mime_multipart{headers=H}, Header) ->
    Msg#mime_multipart{headers=H++[Header]};

add_header(Part = #mime_part{headers=H}, Header) ->
    Part#mime_part{headers=H++[Header]}.

create_attachment(Filename, ContentType) ->
    create_attachment(Filename, ContentType, undefined).

create_attachment(Filename, ContentType, Type) ->
    create_attachment(Filename, ContentType, Type, filename:basename(Filename)).

create_attachment(Filename, ContentType, Type, Name) ->
    Part0 = create_attachment_part(Filename, ContentType, Type, Name),
    ContentId = invent_content_id(),
    Part1 = add_header(Part0, {"Content-ID", ["<" ++ ContentId ++ ">"]}),
    {Part1, ContentId}.

create_attachment_part(Filename, ContentType, Type, Name) ->
    {ok, Data} = file:read_file(Filename),
    DataEncoded = split_line(base64:encode_to_string(Data)),
    #mime_part{name=Name, data=DataEncoded, type=Type, encoding={"base64", ContentType, undefined}}.


%%====================================================================
%% Internal functions
%%====================================================================

encode_header({Header, [V|Vs]}) when is_list(V) ->
    Hdr = lists:map(fun ({K, Value}) when is_list(K), is_list(Value) ->
                            K ++ "=" ++ Value;
                        ({K, Value}) when is_atom(K), is_list(Value) ->
                            atom_to_list(K) ++ "=" ++ Value;
                        (Value) when is_list(Value) -> Value
                    end,
                    [V|Vs]),
    Header ++ ": " ++ join(Hdr, ";\r\n  ");
encode_header({Header, Value}) when Header =:= "To"; Header =:= "From"; Header =:= "Reply-To"; Header =:= "Cc"; Header =:= "Bcc" ->
    % Assume e-mail headers are already encoded
    Header ++ ": " ++ Value;
encode_header({Header, Value}) when is_list(Header), is_list(Value) ->
    % Encode all other headers according to rfc2047
    Header ++ ": " ++ rfc2047:encode(Value);
encode_header({Header, Value}) when is_atom(Header), is_list(Value) ->
    atom_to_list(Header) ++ ": " ++ rfc2047:encode(Value).

encode_headers(PropList) ->
    join(lists:map(fun encode_header/1, PropList), "\r\n").

encode_parts(#mime_multipart{parts=Parts, boundary=Boundary}) ->
    lists:map(fun (P) -> encode_part(P,Boundary) end, Parts).

encode_part(#mime_multipart{} = P, Boundary) ->
    "--" ++ Boundary ++ "\r\n" ++ encode(P);
encode_part(#mime_part{data=Data, headers=Headers} = P, Boundary) ->
    "--" ++ Boundary ++ "\r\n" ++
    encode_headers(part_headers(P)++Headers) ++ "\r\n\r\n" ++
    double_dot(z_convert:to_list(Data)) ++ "\r\n".

part_headers(#mime_part{type=undefined, encoding={Enc, MimeType, undefined},
                        name=Name}) ->
    Nm = case Name of 
             undefined -> [];
             Name -> "name=" ++ Name
         end,
    [{"Content-Transfer-Encoding", Enc},
     {"Content-Type", [MimeType, Nm]}];
 part_headers(#mime_part{type=undefined, encoding={Enc, MimeType, Charset},
                         name=Name}) ->
     Nm = case Name of 
              undefined -> [];
              Name -> ",name=" ++ Name
          end,
     [{"Content-Transfer-Encoding", Enc},
      {"Content-Type", [MimeType, "charset=" ++ Charset ++ Nm]}];
part_headers(#mime_part{type=Type, encoding={Enc, MimeType, undefined},
                      name=Name}) when Type==inline; Type == attachment ->
  [{"Content-Transfer-Encoding", Enc},
   {"Content-Type", [MimeType, "name=" ++ Name]},
   {"Content-Disposition", [atom_to_list(Type), 
                            {"filename", 
                            Name}]}];
part_headers(#mime_part{type=Type, encoding={Enc, MimeType, Charset},
                        name=Name}) when Type==inline; Type == attachment ->
    [{"Content-Transfer-Encoding", Enc},
     {"Content-Type", [MimeType, "charset=" ++ Charset ++ ",name=" ++ Name]},
     {"Content-Disposition", [atom_to_list(Type), 
                              {"filename", 
                              Name}]}].

headers(#mime_multipart{multipart_type=T, headers=H, boundary=Boundary}) ->
    H ++ [{"Content-Type", [multipart_type(T),
                            "boundary=\"" ++ Boundary ++ "\""]}].

multipart_type(mixed) -> "multipart/mixed";
multipart_type(alternative) -> "multipart/alternative";
multipart_type(related) -> "multipart/related".


invent_mime_boundary() ->
    string:copies("=", 10) ++ list_rand(boundary_chars(), 30).

invent_content_id() ->
    list_rand(boundary_chars(), 30).

list_rand(List, N) ->
    lists:map(fun (_) -> list_rand(List) end,
              lists:seq(1,N)).

list_rand(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List).

boundary_chars() ->
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789"
%    "'()+_,-./=?"
    .

%% Double dots at the start of a line, conform to Section 4.5.2 of RFC 2821
double_dot([$.|T]) ->
    double_dot(T, [$., $.]);
double_dot(L) ->
    double_dot(L, []).

    double_dot([], Acc) ->
        lists:reverse(Acc);
    double_dot([13, 10, $. | T], Acc) ->
        double_dot(T, [$., $., 10, 13|Acc]);
    double_dot([H|T], Acc) ->
        double_dot(T, [H|Acc]).


join([H1, H2| T], S) when is_list(H1), is_list(H2), is_list(S) ->
    H1 ++ S ++ join([H2| T], S);
%join([C1, C2 | Chars], S) when is_integer(C1), is_integer(C2), is_list(S) ->
%    [C1|S] ++ S ++ join([C2 | Chars], S);
join([H], _) ->
    H;
join([], _) ->
    [].


%%====================================================================
%% Test functions
%%====================================================================

test_msg() ->
    #mime_multipart{boundary=invent_mime_boundary(),
              headers=[{"To", "Geoff Cant <geoff@example.com>"},
                       {"Subject", "Daily Report"},
                       {"From", "Geoff Cant <geoff@example.com>"},
                       {"Date", httpd_util:rfc1123_date()}
                      ],
              parts=[#mime_part{data="This is a test..."},
                     #mime_part{data="This,is,a,test\r\nof,something,ok,maybe",
                                type=attachment,
                                encoding={"8bit","text/plain","utf-8"},
                                name="foo.csv"}]}.
test() ->
    io:format("~s~n", [encode(test_msg())]).

send(Ip, Host, From, To, Msg=#mime_multipart{}) ->
    ok = smtpc:sendmail(Ip, Host, From, To, encode(Msg)).

send_test(Ip, Host, From, To) ->
    send(Ip, Host, From, To, test_msg()).



%% @doc Encode a string as quoted printable.
%% @spec encode(iolist()) -> binary()
split_line(L) when is_list(L) ->
    split_line(iolist_to_binary(L));
split_line(B) when is_binary(B) ->
    split_line(B, 0, <<>>).

split_line(<<>>, _, Acc) ->
    Acc;
split_line(B, Len, Acc) when Len >= 72 ->
    split_line(B, 0, <<Acc/binary, 13, 10>>);
split_line(<<C,Rest/binary>>, Len, Acc) ->
    split_line(Rest, Len+1, <<Acc/binary, C>>).
