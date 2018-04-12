%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% @author Marc Worrell <marc@worrell.nl>
%% Date: 2009-05-13
%%
%% @doc Parse multipart/form-data request bodies. Uses a callback function to receive the next parts, can call
%% a progress function to report back the progress on receiving the data.
%%
%% Adapted from mochiweb_multipart.erl, integrated with webmachine and zotonic

%% This is the MIT license.
%%
%% Copyright (c) 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
%% INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
%% PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
%% OR OTHER DEALINGS IN THE SOFTWARE.


-module(z_parse_multipart).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
   recv_parse/1,

   find_boundary/2
]).

-include("zotonic.hrl").

-define(CHUNKSIZE, 4096).

-record(mp, {
        boundary :: binary(),
        content_length :: integer(),
        length :: integer(),
        percentage = 0 :: pos_integer(),
        buffer :: binary(),
        next_chunk :: more | ok,
        form = #multipart_form{},
        % z_msg = undefined :: #z_msg_v1{},
        context :: #context{}
    }).


%% @doc Receive and parse the form data in the request body.
%% The progress function should accept the parameters [Percentage, Context]
-spec recv_parse(#context{}) -> {#multipart_form{}, #context{}}.
recv_parse(Context) ->
    {Form, ContextParsed} = parse_multipart_request(Context),
    if Form#multipart_form.file =/= undefined ->
        % Premature end
        file:close(Form#multipart_form.file);
    true ->
        nop
    end,
    {Form, ContextParsed}.


%% @doc Parse the multipart request
parse_multipart_request(Context) ->
    Length = binary_to_integer(cowmachine_req:get_req_header(<<"content-length">>, Context)),
    Boundary = get_boundary(cowmachine_req:get_req_header(<<"content-type">>, Context)),
    Prefix = <<"\r\n--", Boundary/binary>>,
    BS = size(Boundary),
    {Next, Chunk, Context1} = cowmachine_req:stream_req_body(?CHUNKSIZE, Context),
    case Chunk of
        <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> ->
            feed_mp(headers, #mp{boundary=Prefix,
                                 length=size(Chunk),
                                 content_length=Length,
                                 buffer=Rest,
                                 form=#multipart_form{},
                                 next_chunk=Next,
                                 context=Context1});
        _ ->
            lager:info(z_context:lager_md(Context), "Could not decode multipart (~p) chunk: ~p", [Boundary, Chunk]),
            throw({stop_request, 400})
    end.


feed_mp(headers, #mp{buffer=Buffer, form=Form} = State) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer) of
        {exact, N} ->
            {State, N};
        _ ->
            S1 = read_more(State),
            %% Assume headers must be less than ?CHUNKSIZE
            case find_in_binary(<<"\r\n\r\n">>, S1#mp.buffer) of
                {exact, N} ->
                    {S1, N};
                _ ->
                    lager:info("Could not decode multipart: headers incomplete or too long: ~p", [S1#mp.buffer]),
                    throw({stop_request, 400})
            end
    end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    Form1 = handle_data({headers, parse_headers(Headers)}, Form),
    feed_mp(body, State1#mp{buffer=Rest, form=Form1});
feed_mp(body, #mp{boundary=Prefix, buffer=Buffer, form=Form} = State) ->
    case find_boundary(Prefix, Buffer) of
        {end_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, _Rest/binary>> = Buffer,
            Form1 = handle_data({body, Data}, Form),
            Form2 = handle_data(body_end, Form1),
            Form3 = handle_data(eof, Form2),
            {Form3, State#mp.context};
        {next_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            Form1 = handle_data({body, Data}, Form),
            Form2 =handle_data(body_end, Form1),
            State1 = State#mp{form=Form2, buffer=Rest},
            % State2 = maybe_z_msg_context(State1),
            feed_mp(headers, State1);
        {maybe, 0} ->
            % Found a boundary, without an ending newline
            case read_more(State) of
                State ->
                    lager:info("Could not decode multipart: incomplete end boundary at: ~p", [Buffer]),
                    throw({stop_request, 400});
                S1 ->
                    feed_mp(body, S1)
            end;
        {maybe, Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            Form1 = handle_data({body, Data}, Form),
            feed_mp(body, read_more(State#mp{form=Form1, buffer=Rest}));
        not_found ->
            Form1 = handle_data({body, Buffer}, Form),
            State1 = read_more(State#mp{form=Form1, buffer= <<>>}),
            feed_maybe_eof(State1)
    end.

feed_maybe_eof(#mp{buffer= <<>>, content_length=ContentLength, length=Length} = State) when ContentLength =:= Length; ContentLength =:= 0 ->
    Form1 = handle_data(eof, State#mp.form),
    {Form1, State#mp.context};
feed_maybe_eof(#mp{buffer= <<>>}) ->
    lager:info("Could not decode multipart: unexpected end and missing end boundary"),
    throw({stop_request, 400});
feed_maybe_eof(#mp{} = State) ->
    feed_mp(body, State).


% maybe_z_msg_context(#mp{
%             z_msg=undefined,
%             form=#multipart_form{args=[{<<"z_msg">>,Data}|RestArgs]} = Form,
%             context=#context{page_pid=undefined} = Context
%         } = State) ->
%     {ok, #z_msg_v1{} = ZMsg, _Rest} = z_transport:data_decode(Data),
%     #z_msg_v1{page_id=PageId, session_id=SessionId} = ZMsg,
%     Context1 = z_transport:maybe_logon(
%                     z_transport:maybe_set_sessions(SessionId, PageId, Context)),
%     Form1 = Form#multipart_form{args=[{<<"z_msg">>, ZMsg}|RestArgs]},
%     State#mp{z_msg=ZMsg, form=Form1, context=Context1};
% maybe_z_msg_context(#mp{} = State) ->
%     State.


%% @doc Report progress back to the page.
progress(0, _ContentLength, _Context) ->
    ok;
% progress(Percentage, ContentLength, Context)
%     when ContentLength > ?CHUNKSIZE*5, TriggerId =/= undefined, TriggerId =/= "", TriggerId =/= <<>> ->
%     case is_push_attached(Context) of
%         true ->
%             JS = iolist_to_binary([
%                         "z_progress('", z_utils:js_escape(TriggerId),
%                             "',", integer_to_list(Percentage),");"
%                     ]),
%             z_transport:page(javascript, JS, Context);
%         false ->
%             ok
%     end;
progress(_Percentage, _ContentLength, _Context) ->
    ok.

% is_push_attached(Context) ->
%     z_session_page:get_attach_state(Context) =:= attached.



%% @doc Callback function collecting all data found in the multipart/form-data body
-spec handle_data({headers, list()}|{body,binary()}|body_end|eof, #multipart_form{}) -> #multipart_form{}.
handle_data({headers, Headers}, Form) ->
    % Find out if it is a file
    ContentDisposition = proplists:get_value(<<"content-disposition">>, Headers),
    case ContentDisposition of
        {<<"form-data">>, Opts} ->
            Name = proplists:get_value(<<"name">>, Opts),
            Filename = proplists:get_value(<<"filename">>, Opts),
            case {Name, Filename} of
                {undefined, undefined} ->
                    Form;
                {Name, undefined} ->
                    Form#multipart_form{name=Name, data= <<>>};
                {Name, Filename} ->
                    ContentLength = case proplists:get_value(<<"content-length">>, Headers) of
                                        undefined -> undefined;
                                        CL when is_integer(CL) -> CL
                                    end,
                    ContentType = case proplists:get_value(<<"content-type">>, Headers) of
                                        undefined -> undefined;
                                        {MimeA, MimeB, _Opts} -> <<MimeA/binary, $/, MimeB/binary>>
                                  end,
                    Form#multipart_form{name=Name,
                                        filename=Filename,
                                        content_length=ContentLength,
                                        content_type=ContentType,
                                        tmpfile=z_tempfile:new()}
            end;
        _ ->
            Form
    end;
handle_data({body, Data}, #multipart_form{filename=undefined} = Form) ->
    Form#multipart_form{data = <<(Form#multipart_form.data)/binary, Data/binary>>};
handle_data({body, Data}, #multipart_form{filename=Filename, file=undefined} = Form) when Filename =/= undefined ->
    case file:open(Form#multipart_form.tmpfile, [raw,write]) of
        {ok, File} ->
            file:write(File, Data),
            Form#multipart_form{file=File};
        {error, Error} ->
            lager:error("Couldn't open ~p for writing, error: ~p~n", [Form#multipart_form.tmpfile, Error]),
            exit(could_not_open_file_for_writing)
    end;
handle_data({body, Data}, #multipart_form{file=File} = Form) when File =/= undefined ->
    file:write(File, Data),
    Form;
handle_data(body_end, #multipart_form{name=undefined} = Form) ->
    Form#multipart_form{
        name=undefined,
        data=undefined
    };
handle_data(body_end, #multipart_form{file=undefined, name=Name} = Form) when Name =/= undefined ->
    Form#multipart_form{
        name=undefined,
        data=undefined,
        args=[
            {Name, Form#multipart_form.data}
            | Form#multipart_form.args
        ]
    };
handle_data(body_end, #multipart_form{file=File, name=Name} = Form) when Name =/= undefined ->
    file:close(File),
    Form#multipart_form{
        name=undefined,
        data=undefined,
        file=undefined,
        tmpfile=undefined,
        filename=undefined,
        content_type=undefined,
        content_length=undefined,
        files=[
            {Name, Form#multipart_form.filename, Form#multipart_form.tmpfile}
            |Form#multipart_form.files
        ]
    };
handle_data(eof, Form) ->
    Form.



%% @doc Read more data for the feed_mp functions.
-spec read_more(#mp{}) -> #mp{}.
read_more(State=#mp{next_chunk=ok, content_length=ContentLength, length=Length} = State) when ContentLength =:= Length ->
    State;
read_more(State=#mp{next_chunk=ok, context=Context} = State) ->
    lager:info(z_context:lager_md(Context), "Multipart post with wrong content length"),
    throw({error, wrong_content_length});
read_more(State=#mp{length=Length, content_length=ContentLength,
                percentage=Percentage,
                buffer=Buffer, next_chunk=more, context=Context}) ->
    {Next1, Data, Context1} = cowmachine_req:stream_req_body(?CHUNKSIZE, Context),
    Buffer1 = <<Buffer/binary, Data/binary>>,
    Length1 = Length + size(Data),
    NewPercentage = case ContentLength of
                        0 -> 100;
                        _ -> (Length1 * 100) div ContentLength
                    end,
    case NewPercentage > Percentage of
        true -> progress(NewPercentage, ContentLength, Context1);
        _ -> nop
    end,
    State#mp{length=Length1, buffer=Buffer1, next_chunk=Next1, percentage=NewPercentage, context=Context1}.


%% @doc Parse the headers of a part in the form data
parse_headers(Binary) ->
    parse_headers(Binary, []).

parse_headers(<<>>, Acc) ->
    Acc;
parse_headers(Binary, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, [split_header(Line) | Acc]);
        not_found ->
            lists:reverse([split_header(Binary) | Acc])
    end.

split_header(Line) ->
    [Name,Value] = binary:split(Line, <<":">>),
    Name1 = z_string:to_lower(z_string:trim(Name)),
    {Name1, parse_header(Name1, z_string:trim(Value))}.

parse_header(<<"content-length">>, Value) ->
    cow_http_hd:parse_content_length(Value);
parse_header(<<"content-type">>, Value) ->
    cow_http_hd:parse_content_type(Value);
parse_header(_Name, Value) ->
    cowmachine_util:parse_header(Value).

%% @doc Get the request boundary separating the parts in the request body
get_boundary(ContentType) ->
    {<<"multipart">>, <<"form-data">>, Opts} = cow_http_hd:parse_content_type(ContentType),
    {<<"boundary">>, Boundary} = proplists:lookup(<<"boundary">>, Opts),
    Boundary.

%% @doc Find the next boundary in the data
find_boundary(Prefix, Data) ->
    case find_in_binary(Prefix, Data) of
        {exact, Skip} ->
            PrefixSkip = Skip + size(Prefix),
            case Data of
                <<_:PrefixSkip/binary, "\r\n", _/binary>> ->
                    {next_boundary, Skip, size(Prefix) + 2};
                <<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
                    {end_boundary, Skip, size(Prefix) + 4};
                % POSTs by Adobe Flash don't have the ending newline
                <<_:PrefixSkip/binary, "--", _/binary>> ->
                    {end_boundary, Skip, size(Prefix) + 2};
                _ when size(Data) < PrefixSkip + 4 ->
                    %% Underflow
                    {maybe, Skip};
                _ ->
                    %% False positive
                    not_found
            end;
        {partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
            %% Underflow
            {maybe, Skip};
        _ ->
            not_found
    end.

find_in_binary(B, Data) when size(B) > 0 ->
    case size(Data) - size(B) of
        Last when Last < 0 ->
            partial_find(B, Data, 0, size(Data));
        Last ->
            find_in_binary(B, size(B), Data, 0, Last)
    end.

find_in_binary(B, BS, D, N, Last) when N =< Last->
    case D of
        <<_:N/binary, B:BS/binary, _/binary>> ->
            {exact, N};
        _ ->
            find_in_binary(B, BS, D, 1 + N, Last)
    end;
find_in_binary(B, BS, D, N, Last) when N =:= 1 + Last ->
    partial_find(B, D, N, BS - 1).

partial_find(_B, _D, _N, 0) ->
    not_found;
partial_find(B, D, N, K) ->
    <<B1:K/binary, _/binary>> = B,
    case D of
        <<_Skip:N/binary, B1:K/binary>> ->
            {partial, N, K};
        _ ->
            partial_find(B, D, 1 + N, K - 1)
    end.

