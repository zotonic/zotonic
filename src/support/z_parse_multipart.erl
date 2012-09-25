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
   recv_parse/2,
   
   find_boundary/2
]).

-include("zotonic.hrl").

-define(CHUNKSIZE, 4096).

-record(mp, {state, boundary, content_length, length, percentage=0, 
			 buffer, next_chunk, callback, progress, context}).


%% @doc Receive and parse the form data in the request body.  
%% The progress function should accept the parameters [Percentage, Context]
%% @spec recv_parse(Context) -> {form(), NewContext}
recv_parse(Context) ->
    recv_parse(fun(_Filename, _ContentType, _Size) -> ok end, Context).

%% @spec recv_parse(UploadCheckFun, Context) -> {form(), NewContext}
recv_parse(UploadCheckFun, Context) ->
    Callback = fun(N) -> callback(N, #multipart_form{}, UploadCheckFun) end,
    {_LengthRemaining, _RestData, Form, ContextParsed} = parse_multipart_request(fun progress/4, Callback, Context),
    if Form#multipart_form.file =/= undefined ->
        % Premature end
        file:close(Form#multipart_form.file);
    true ->
        nop
    end,
    {Form, ContextParsed}.


%% @doc Report progress back to the page.
progress(Percentage, ContentLength, _ReceivedLength, Context) when ContentLength > ?CHUNKSIZE*5 ->
	case {	z_convert:to_bool(z_context:get_q("z_comet", Context)),
			z_context:get_q("z_pageid", Context), 
			z_context:get_q("z_trigger_id", Context)} of
		{true, PageId, TriggerId} when PageId /= undefined; TriggerId /= undefined ->
			ContextEnsured = z_context:ensure_all(Context),
			z_session_page:add_script("z_progress('"
						++z_utils:js_escape(TriggerId)++"',"
						++integer_to_list(Percentage)++");", ContextEnsured);
		_ -> nop
	end;
progress(_, _, _, _) ->
    nop.
	

%% @doc Callback function collecting all data found in the multipart/form-data body
%% @spec callback(Next, function(), form()) -> function() | form()
callback(Next, Form, UploadCheckFun) ->
    case Next of
        {headers, Headers} ->
            % Find out if it is a file
            ContentDisposition = proplists:get_value("content-disposition", Headers),
            case ContentDisposition of
                {"form-data", [{"name", Name}, {"filename",Filename}]} ->
                    ContentLength = case proplists:get_value("content-length", Headers) of
                                        undefined -> undefined;
                                        {CL,_} -> z_convert:to_integer(CL)
                                    end,
                    ContentType = case proplists:get_value("content-type", Headers) of
                                        undefined -> undefined;
                                        {Mime,_} -> Mime
                                  end,
                    case UploadCheckFun(Filename, ContentType, ContentLength) of
                        ok ->
                            NF = Form#multipart_form{name=Name,
                                                     filename=Filename, 
                                                     content_length=ContentLength, 
                                                     content_type=ContentType,
                                                     tmpfile=z_tempfile:new()},
                            fun(N) -> callback(N, NF, UploadCheckFun) end;
                        {error, _Reason} = Error ->
                            throw(Error)
                    end;
                {"form-data",[{"name",Name}]} ->
                    NF = Form#multipart_form{name=Name, data=[]},
                    fun(N) -> callback(N, NF, UploadCheckFun) end;
                _ ->
                    fun(N) -> callback(N, Form, UploadCheckFun) end
            end;

        {body, Data} ->
            if  Form#multipart_form.filename =/= undefined ->
                if Form#multipart_form.file =/= undefined ->
                    file:write(Form#multipart_form.file, Data),
                    NewForm = Form;
                true ->
                    case file:open(Form#multipart_form.tmpfile, [raw,write]) of
                        {ok, File} ->
                            file:write(File, Data),
                            NewForm = Form#multipart_form{file=File};
                        {error, Error} ->
                            ?ERROR("Couldn't open ~p for writing, error: ~p~n", [Form#multipart_form.tmpfile, Error]),
                            NewForm = Form,
                            exit(could_not_open_file_for_writing)
                    end
                end;
            true ->
                NewForm = Form#multipart_form{data=[Form#multipart_form.data, binary_to_list(Data)]}
            end,
            fun(N) -> callback(N, NewForm, UploadCheckFun) end;

         body_end ->
            NewForm = if Form#multipart_form.file =/= undefined ->
                            file:close(Form#multipart_form.file),
                            Form#multipart_form{
                                name=undefined,
                                data=undefined,
                                file=undefined,
                                tmpfile=undefined,
                                filename=undefined,
                                content_type=undefined,
                                content_length=undefined,
                                files=[{Form#multipart_form.name, Form#multipart_form.filename, Form#multipart_form.tmpfile}|Form#multipart_form.files]
                            };
                        Form#multipart_form.name =/= undefined ->
                            Data = lists:flatten(Form#multipart_form.data),
                            Form#multipart_form{
                                name=undefined,
                                data=undefined,
                                args=[{Form#multipart_form.name, Data} | Form#multipart_form.args]
                            };
                        true ->
                            Form
                        end,
            fun(N) -> callback(N, NewForm, UploadCheckFun) end;
        
        eof ->
            Form
    end.


%% @doc Parse the multipart request
parse_multipart_request(ProgressFunction, Callback, Context) ->
    ReqData  = z_context:get_reqdata(Context),
    Length   = list_to_integer(wrq:get_req_header_lc("content-length", ReqData)),
    Boundary = iolist_to_binary(get_boundary(wrq:get_req_header_lc("content-type", ReqData))),
    Prefix = <<"\r\n--", Boundary/binary>>,
    BS = size(Boundary),
    {{Chunk, Next}, ReqData1} = wrq:stream_req_body(ReqData, ?CHUNKSIZE),
    Context1 = z_context:set_reqdata(ReqData1, Context),
    <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk,
    feed_mp(headers, #mp{boundary=Prefix,
                         length=size(Chunk),
                         content_length=Length,
                         buffer=Rest,
                         callback=Callback,
                         progress=ProgressFunction,
                         next_chunk=Next,
                         context=Context1}).


feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer) of
        {exact, N} ->
            {State, N};
        _ ->
           S1 = read_more(State),
           %% Assume headers must be less than ?CHUNKSIZE
           {exact, N} = find_in_binary(<<"\r\n\r\n">>, S1#mp.buffer),
           {S1, N}
    end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    NextCallback = Callback({headers, parse_headers(Headers)}),
    feed_mp(body, State1#mp{buffer=Rest, callback=NextCallback});

feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}) ->
    case find_boundary(Prefix, Buffer) of
        {end_boundary, Start, Skip} ->
             <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
             C1 = Callback({body, Data}),
             C2 = C1(body_end),
             {State#mp.length, Rest, C2(eof), State#mp.context};
        {next_boundary, Start, Skip} ->
             <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
             C1 = Callback({body, Data}),
             feed_mp(headers, State#mp{callback=C1(body_end), buffer=Rest});
        {maybe, 0} ->
            % Found a boundary, without an ending newline
            case read_more(State) of
                State -> throw({error, incomplete_end_boundary});
                S1 -> feed_mp(body, S1)
            end;
        {maybe, Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}), buffer=Rest}));
        not_found ->
            {Data, Rest} = {Buffer, <<>>},
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}), buffer=Rest}))
    end.



%% @doc Read more data for the feed_mp functions.
%% @spec read_more(mp()) -> mp()
read_more(State=#mp{next_chunk=done, content_length=ContentLength, length=Length} = State) when ContentLength =:= Length ->
    State;
read_more(State=#mp{next_chunk=done} = State) ->
    throw({error, wrong_content_length});
read_more(State=#mp{length=Length, content_length=ContentLength, 
				percentage=Percentage,
				buffer=Buffer, next_chunk=Next, context=Context,
				progress=ProgressFunction}) ->
    {Data, Next1} = Next(),
    Buffer1 = <<Buffer/binary, Data/binary>>,
	Length1 = Length + size(Data),
	NewPercentage = case ContentLength of
						0 -> 100;
						_ -> (Length1 * 100) div ContentLength
					end,
	case NewPercentage > Percentage of
		true ->
			case ProgressFunction of
				undefined -> nop;
				F -> F(NewPercentage, ContentLength, Length1, Context)
			end;
		_ ->
			nop
	end,
    State#mp{length=Length1, buffer=Buffer1, next_chunk=Next1, percentage=NewPercentage}.


%% @doc Parse the headers of a part in the form data
parse_headers(<<>>) ->
    [];
parse_headers(Binary) ->
    parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, [split_header(Line) | Acc]);
        not_found ->
            lists:reverse([split_header(Binary) | Acc])
    end.

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end, binary_to_list(Line)),
    {string:to_lower(string:strip(Name)), mochiweb_util:parse_header(Value)}.


%% @doc Get the request boundary separating the parts in the request body
get_boundary(ContentType) ->
    {"multipart/form-data", Opts} = mochiweb_util:parse_header(ContentType),
    case proplists:get_value("boundary", Opts) of
        S when is_list(S) ->
            S
    end.

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

