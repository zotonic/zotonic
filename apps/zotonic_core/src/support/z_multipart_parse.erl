%% @author Bob Ippolito, Marc Worrell
%% @copyright 2007 Mochi Media, Inc; 2009-2026 Marc Worrell
%% @doc Parse multipart/form-data request bodies. Uses a callback function to receive the next parts, can call
%% a progress function to report back the progress on receiving the data.
%% The parser applies size and count limits from site configuration, falling back to the global z_config values.
%%
%% Supported configuration keys:
%%
%% - formdata_max_boundary_length - maximum boundary length in bytes. This limit is always enforced;
%%   invalid, zero, or unset values fall back to the global z_config default.
%% - formdata_max_content_length - maximum total multipart request body size in bytes, checked against
%%   the declared Content-Length and against streamed bytes while reading.
%% - formdata_max_field_length - maximum size in bytes for a single non-file form field.
%% - formdata_max_form_data_length - maximum combined size in bytes for all non-file form field data.
%%   File bodies written to tempfiles are not counted in this limit.
%% - formdata_max_file_length - maximum size in bytes for a single uploaded file part.
%% - formdata_max_files - maximum number of uploaded file parts.
%% - formdata_max_fields - maximum number of non-file form fields.
%%
%% For all limits except formdata_max_boundary_length, the atom none disables the limit.
%% Empty or undefined site values use the corresponding global z_config value. Exceeding a size or count limit
%% rejects the request with HTTP 413; malformed multipart input and overlong boundaries reject with HTTP 400.
%% The #acl_max_upload_size{} notification can override the effective file-size limit for the current user,
%% including anonymous users. When ACL returns a maximum upload size, the parser derives a conservative file count
%% and request body limit from that value, allowing one very large file or several smaller files per form post.
%%
%% Adapted from mochiweb_multipart.erl, integrated with webmachine and zotonic
%% @end

%% This is the MIT license.
%%
%% Copyright (c) 2007 Mochi Media, Inc.
%% Copyright (c) 2009-2026 Marc Worrell
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


-module(z_multipart_parse).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
   recv_parse/1
]).

-include("zotonic.hrl").

-define(CHUNKSIZE, 4096).
-type parse_config() :: #{
        max_boundary_length := pos_integer(),
        max_content_length := undefined | pos_integer(),
        max_field_length := undefined | pos_integer(),
        max_form_data_length := undefined | pos_integer(),
        max_file_length := undefined | pos_integer(),
        max_files := undefined | pos_integer(),
        max_fields := undefined | pos_integer()
    }.

-record(mp, {
        boundary :: binary(),
        config :: parse_config(),
        content_length :: integer(),
        length :: integer(),
        percentage = 0 :: non_neg_integer(),
        form_data_length = 0 :: non_neg_integer(),
        field_length = 0 :: non_neg_integer(),
        file_length = 0 :: non_neg_integer(),
        field_count = 0 :: non_neg_integer(),
        file_count = 0 :: non_neg_integer(),
        buffer :: binary(),
        next_chunk :: more | ok,
        form = #multipart_form{},
        context :: #context{}
    }).


%% @doc Receive and parse the form data in the request body.
%% The progress function should accept the parameters [Percentage, Context]
-spec recv_parse( z:context() ) -> {#multipart_form{}, z:context()}.
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
    Config = init_config(Context),
    case cowmachine_req:get_req_header(<<"content-length">>, Context) of
        undefined ->
            z_context:logger_md(Context),
            ?LOG_NOTICE(#{
                in => zotonic_core,
                text => <<"Could not decode multipart: content-length header undefined">>,
                result => error,
                reason => content_length_undefined
            }),
            throw({stop_request, 400});
        ContentLength ->
            Length = binary_to_integer(ContentLength),
            check_limit(max_content_length, Length, Config, Context),
            Boundary = get_boundary(cowmachine_req:get_req_header(<<"content-type">>, Context), Config),
            Prefix = <<"\r\n--", Boundary/binary>>,
            BS = size(Boundary),
            {Next, Chunk, Context1} = cowmachine_req:stream_req_body(?CHUNKSIZE, Context),
            case Chunk of
                <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> ->
                    feed_mp(headers, #mp{boundary = Prefix,
                                         config = Config,
                                         length = size(Chunk),
                                         content_length = Length,
                                         buffer = Rest,
                                         form = #multipart_form{},
                                         next_chunk = Next,
                                         context = Context1});
                _ ->
                    z_context:logger_md(Context),
                    ?LOG_NOTICE(#{
                        text => <<"Could not decode multipart chunk">>,
                        in => zotonic_core,
                        boundary => Boundary,
                        chunk => Chunk
                    }),
                    throw({stop_request, 400})
            end
    end.


feed_mp(headers, #mp{buffer=Buffer, config=Config} = State) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer, Config) of
        {exact, N} ->
            {State, N};
        _ ->
            S1 = read_more(State),
            %% Assume headers must be less than ?CHUNKSIZE
            case find_in_binary(<<"\r\n\r\n">>, S1#mp.buffer, Config) of
                {exact, N} ->
                    {S1, N};
                _ ->
                    ?LOG_NOTICE(#{
                        text => <<"Could not decode multipart: headers incomplete or too long">>,
                        in => zotonic_core,
                        result => error,
                        reason => headers_incomplete_or_too_long,
                        buffer => S1#mp.buffer
                    }),
                    throw({stop_request, 400})
            end
    end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    feed_mp(body, handle_headers(parse_headers(Headers, Config), State1#mp{buffer=Rest}));
feed_mp(body, #mp{boundary=Prefix, buffer=Buffer, config=Config} = State) ->
    case find_boundary(Prefix, Buffer, Config) of
        {end_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, _Rest/binary>> = Buffer,
            State1 = handle_body(Data, State),
            State2 = handle_body_end(State1),
            Form3 = handle_data(eof, State2#mp.form, Config),
            {Form3, State2#mp.context};
        {next_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            State1 = handle_body_end(handle_body(Data, State)),
            State2 = State1#mp{buffer=Rest},
            % State2 = maybe_z_msg_context(State1),
            feed_mp(headers, State2);
        {'maybe', 0} ->
            % Found a boundary, without an ending newline
            case read_more(State) of
                State ->
                    ?LOG_NOTICE(#{
                        text => <<"Could not decode multipart: incomplete end boundary">>,
                        in => zotonic_core,
                        result => error,
                        reason => incomplete_end_boundary,
                        buffer => Buffer
                    }),
                    throw({stop_request, 400});
                S1 ->
                    feed_mp(body, S1)
            end;
        {'maybe', Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            feed_mp(body, read_more((handle_body(Data, State))#mp{buffer=Rest}));
        not_found ->
            State1 = read_more((handle_body(Buffer, State))#mp{buffer= <<>>}),
            feed_maybe_eof(State1)
    end.

feed_maybe_eof(#mp{buffer= <<>>, content_length=ContentLength, length=Length} = State) when ContentLength =:= Length; ContentLength =:= 0 ->
    Form1 = handle_data(eof, State#mp.form, State#mp.config),
    {Form1, State#mp.context};
feed_maybe_eof(#mp{buffer= <<>>}) ->
    ?LOG_NOTICE(#{
        in => zotonic_core,
        text => <<"Could not decode multipart: unexpected end and missing end boundary">>,
        result => error,
        reason => unexpected_end_and_missing_end_boundary
    }),
    throw({stop_request, 400});
feed_maybe_eof(#mp{} = State) ->
    feed_mp(body, State).


%% @doc Report progress back to the page.
progress(0, _ContentLength, _Form, _Context) ->
    ok;
progress(Percentage, ContentLength, Form, Context) when ContentLength > ?CHUNKSIZE*5 ->
    case proplists:get_value(<<"zotonic_topic_progress">>, Form#multipart_form.args) of
        Topic when is_binary(Topic) ->
            z_mqtt:publish(
                Topic,
                #{
                    form_id => proplists:get_value(<<"z_trigger_id">>, Form#multipart_form.args),
                    percentage => Percentage,
                    content_length => ContentLength
                },
                Context);
        _ ->
            ok
    end;
progress(_Percentage, _ContentLength, _Form, _Context) ->
    ok.


%% @doc Callback function collecting all data found in the multipart/form-data body
-spec handle_data({headers, list()}|{body,binary()}|body_end|eof, #multipart_form{}, parse_config()) -> #multipart_form{}.
handle_data({headers, Headers}, Form, _Config) ->
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
                    {ok, {TmpPid, TmpFile}} = z_tempfile:monitored_new(),
                    Form#multipart_form{name = Name,
                                        filename = Filename,
                                        content_length = ContentLength,
                                        content_type = ContentType,
                                        tmpfile = TmpFile,
                                        tmpmonitor = TmpPid}
            end;
        _ ->
            Form
    end;
handle_data({body, Data}, #multipart_form{filename=undefined} = Form, _Config) ->
    Form#multipart_form{data = <<(Form#multipart_form.data)/binary, Data/binary>>};
handle_data({body, Data}, #multipart_form{filename=Filename, file=undefined} = Form, _Config) when Filename =/= undefined ->
    case file:open(Form#multipart_form.tmpfile, [raw,write]) of
        {ok, File} ->
            file:write(File, Data),
            Form#multipart_form{file=File};
        {error, Error} ->
            ?LOG_ERROR(#{
                text => <<"Could not open file for writing">>,
                in => zotonic_core,
                result => error,
                reason => Error,
                file => Form#multipart_form.tmpfile
            }),
            exit(could_not_open_file_for_writing)
    end;
handle_data({body, Data}, #multipart_form{file=File} = Form, _Config) when File =/= undefined ->
    file:write(File, Data),
    Form;
handle_data(body_end, #multipart_form{name=undefined} = Form, _Config) ->
    Form#multipart_form{
        name = undefined,
        data = undefined
    };
handle_data(body_end, #multipart_form{file=undefined, name=Name} = Form, _Config) when Name =/= undefined ->
    Form#multipart_form{
        name = undefined,
        data = undefined,
        args = [
            {Name, Form#multipart_form.data}
            | Form#multipart_form.args
        ]
    };
handle_data(body_end, #multipart_form{file=File, name=Name} = Form, _Config) when Name =/= undefined ->
    file:close(File),
    Form#multipart_form{
        name = undefined,
        data = undefined,
        file = undefined,
        tmpfile = undefined,
        filename = undefined,
        content_type = undefined,
        content_length = undefined,
        files = [
            {Name, Form#multipart_form.filename, Form#multipart_form.tmpfile, Form#multipart_form.tmpmonitor}
            | Form#multipart_form.files
        ]
    };
handle_data(eof, Form, _Config) ->
    Form.

handle_headers(Headers, #mp{form=Form, config=Config} = State) ->
    check_part_headers(Headers, State),
    Form1 = handle_data({headers, Headers}, Form, Config),
    State#mp{
        form = Form1,
        field_length = 0,
        file_length = 0
    }.

check_part_headers(Headers, #mp{config=Config} = State) ->
    case proplists:get_value(<<"content-disposition">>, Headers) of
        {<<"form-data">>, Opts} ->
            Name = proplists:get_value(<<"name">>, Opts),
            Filename = proplists:get_value(<<"filename">>, Opts),
            check_part_headers(Name, Filename, Headers, State, Config);
        _ ->
            ok
    end.

check_part_headers(undefined, _Filename, _Headers, _State, _Config) ->
    ok;
check_part_headers(_Name, undefined, Headers, State, Config) ->
    check_limit(max_fields, State#mp.field_count + 1, Config, State#mp.context),
    maybe_check_part_content_length(max_field_length, Headers, State);
check_part_headers(_Name, _Filename, Headers, State, Config) ->
    check_limit(max_files, State#mp.file_count + 1, Config, State#mp.context),
    maybe_check_part_content_length(max_file_length, Headers, State).

maybe_check_part_content_length(Key, Headers, #mp{config=Config, context=Context}) ->
    case proplists:get_value(<<"content-length">>, Headers) of
        N when is_integer(N) ->
            check_limit(Key, N, Config, Context);
        _ ->
            ok
    end.

handle_body(Data, #mp{form=#multipart_form{filename=undefined}, config=Config} = State) ->
    DataLength = size(Data),
    FieldLength = State#mp.field_length + DataLength,
    FormDataLength = State#mp.form_data_length + DataLength,
    check_limit(max_field_length, FieldLength, Config, State#mp.context),
    check_limit(max_form_data_length, FormDataLength, Config, State#mp.context),
    Form1 = handle_data({body, Data}, State#mp.form, Config),
    State#mp{
        form = Form1,
        field_length = FieldLength,
        form_data_length = FormDataLength
    };
handle_body(Data, #mp{config=Config} = State) ->
    FileLength = State#mp.file_length + size(Data),
    check_limit(max_file_length, FileLength, Config, State#mp.context),
    Form1 = handle_data({body, Data}, State#mp.form, Config),
    State#mp{
        form = Form1,
        file_length = FileLength
    }.

handle_body_end(#mp{form=#multipart_form{file=undefined, name=Name}, config=Config} = State) when Name =/= undefined ->
    FieldCount = State#mp.field_count + 1,
    check_limit(max_fields, FieldCount, Config, State#mp.context),
    Form1 = handle_data(body_end, State#mp.form, Config),
    State#mp{
        form = Form1,
        field_count = FieldCount,
        field_length = 0
    };
handle_body_end(#mp{form=#multipart_form{file=File, name=Name}, config=Config} = State) when File =/= undefined, Name =/= undefined ->
    FileCount = State#mp.file_count + 1,
    check_limit(max_files, FileCount, Config, State#mp.context),
    Form1 = handle_data(body_end, State#mp.form, Config),
    State#mp{
        form = Form1,
        file_count = FileCount,
        file_length = 0
    };
handle_body_end(#mp{config=Config} = State) ->
    Form1 = handle_data(body_end, State#mp.form, Config),
    State#mp{
        form = Form1,
        field_length = 0,
        file_length = 0
    }.



%% @doc Read more data for the feed_mp functions.
-spec read_more(#mp{}) -> #mp{}.
read_more(State=#mp{next_chunk=ok, content_length=ContentLength, length=Length} = State) when ContentLength =:= Length ->
    State;
read_more(State=#mp{next_chunk=ok, context=Context} = State) ->
    z_context:logger_md(Context),
    ?LOG_NOTICE(#{
        in => zotonic_core,
        text => <<"Multipart post with wrong content length">>,
        result => error,
        reason => wrong_content_length
    }),
    throw({error, wrong_content_length});
read_more(State=#mp{length=Length, content_length=ContentLength,
                percentage=Percentage, config=Config,
                buffer=Buffer, next_chunk=more, context=Context}) ->
    {Next1, Data, Context1} = cowmachine_req:stream_req_body(?CHUNKSIZE, Context),
    Buffer1 = <<Buffer/binary, Data/binary>>,
    Length1 = Length + size(Data),
    check_limit(max_content_length, Length1, Config, Context1),
    NewPercentage = case ContentLength of
                        0 -> 100;
                        _ -> (Length1 * 100) div ContentLength
                    end,
    case NewPercentage > Percentage of
        true -> progress(NewPercentage, ContentLength, State#mp.form, Context1);
        _ -> nop
    end,
    State#mp{length=Length1, buffer=Buffer1, next_chunk=Next1, percentage=NewPercentage, context=Context1}.


%% @doc Parse the headers of a part in the form data
parse_headers(Binary, Config) ->
    parse_headers(Binary, Config, []).

parse_headers(<<>>, _Config, Acc) ->
    Acc;
parse_headers(Binary, Config, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary, Config) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, Config, [split_header(Line) | Acc]);
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
get_boundary(ContentType, Config) ->
    {<<"multipart">>, <<"form-data">>, Opts} = cow_http_hd:parse_content_type(ContentType),
    {<<"boundary">>, Boundary} = proplists:lookup(<<"boundary">>, Opts),
    case size(Boundary) =< maps:get(max_boundary_length, Config) of
        true ->
            Boundary;
        false ->
            throw({stop_request, 400})
    end.

%% @doc Find the next boundary in the data
find_boundary(Prefix, Data, Config) ->
    check_boundary_length(Prefix, Config),
    case find_in_binary(Prefix, Data, Config) of
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
                    {'maybe', Skip};
                _ ->
                    %% False positive
                    not_found
            end;
        {partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
            %% Underflow
            {'maybe', Skip};
        _ ->
            not_found
    end.

find_in_binary(B, Data, Config) when size(B) > 0 ->
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

check_boundary_length(Prefix, Config) ->
    MaxBoundaryLength = maps:get(max_boundary_length, Config),
    case size(Prefix) =< MaxBoundaryLength + 4 of
        true ->
            ok;
        false ->
            throw({stop_request, 400})
    end.

check_limit(Key, Value, Config, Context) ->
    case maps:get(Key, Config) of
        undefined ->
            ok;
        Max when Value =< Max ->
            ok;
        Max ->
            log_limit_exceeded(Key, Value, Max, Context),
            throw({stop_request, 413})
    end.

log_limit_exceeded(Key, Value, Max, Context) ->
    case Context of
        undefined -> ok;
        #context{} -> z_context:logger_md(Context)
    end,
    ?LOG_NOTICE(#{
        in => zotonic_core,
        text => <<"Could not decode multipart: form-data limit exceeded">>,
        result => error,
        reason => Key,
        size => Value,
        max_size => Max
    }).

init_config(Context) ->
    Config = #{
        max_boundary_length => config_required_limit(formdata_max_boundary_length, Context),
        max_content_length => config_optional_limit(formdata_max_content_length, Context),
        max_field_length => config_optional_limit(formdata_max_field_length, Context),
        max_form_data_length => config_optional_limit(formdata_max_form_data_length, Context),
        max_file_length => config_optional_limit(formdata_max_file_length, Context),
        max_files => config_optional_limit(formdata_max_files, Context),
        max_fields => config_optional_limit(formdata_max_fields, Context)
    },
    apply_acl_upload_size(Context, Config).

apply_acl_upload_size(#context{} = Context, Config) ->
    case acl_max_upload_size(Context) of
        undefined ->
            Config;
        MaxUploadSize ->
            MaxFiles = max_files_for_upload_size(MaxUploadSize, Config),
            Config#{
                max_content_length => max_content_length_for_upload_size(MaxUploadSize, MaxFiles, Config),
                max_file_length => MaxUploadSize,
                max_files => MaxFiles
            }
    end;
apply_acl_upload_size(_Context, Config) ->
    Config.

acl_max_upload_size(Context) ->
    normalize_optional_limit(z_notifier:first(#acl_max_upload_size{}, Context)).

max_files_for_upload_size(MaxUploadSize, Config) ->
    BandFiles = max_files_for_upload_size_1(MaxUploadSize, Config),
    FittingFiles = max_files_fit_in_content_length(MaxUploadSize, Config),
    ConfigMaxFiles = maps:get(max_files, Config),
    maybe_cap_max_files(erlang:max(BandFiles, FittingFiles), ConfigMaxFiles).

max_files_for_upload_size_1(MaxUploadSize, _Config) when MaxUploadSize >= 1024 * 1024 * 1024 ->
    2;
max_files_for_upload_size_1(MaxUploadSize, _Config) when MaxUploadSize >= 256 * 1024 * 1024 ->
    4;
max_files_for_upload_size_1(MaxUploadSize, _Config) when MaxUploadSize >= 100 * 1024 * 1024 ->
    8;
max_files_for_upload_size_1(_MaxUploadSize, Config) ->
    case maps:get(max_files, Config) of
        undefined -> 1;
        MaxFiles -> MaxFiles
    end.

max_files_fit_in_content_length(MaxUploadSize, Config) ->
    case maps:get(max_content_length, Config) of
        undefined ->
            1;
        MaxContentLength ->
            FormDataLength = optional_limit(maps:get(max_form_data_length, Config)),
            HeaderAllowance = 65536,
            SpaceForFiles = MaxContentLength - FormDataLength - HeaderAllowance,
            erlang:max(1, SpaceForFiles div (MaxUploadSize + HeaderAllowance))
    end.

maybe_cap_max_files(MaxFiles, undefined) ->
    MaxFiles;
maybe_cap_max_files(MaxFiles, ConfigMaxFiles) ->
    erlang:min(MaxFiles, ConfigMaxFiles).

max_content_length_for_upload_size(MaxUploadSize, MaxFiles, Config) ->
    FormDataLength = optional_limit(maps:get(max_form_data_length, Config)),
    HeaderAllowance = (MaxFiles + 1) * 65536,
    (MaxUploadSize * MaxFiles) + FormDataLength + HeaderAllowance.

optional_limit(undefined) -> 0;
optional_limit(N) -> N.

config_required_limit(Key, Context) ->
    case config_optional_limit(Key, Context) of
        N when is_integer(N), N > 0 ->
            N;
        undefined ->
            z_config:get(Key)
    end.

config_optional_limit(Key, Context) ->
    normalize_optional_limit(config_value(Key, Context)).

config_value(Key, undefined) ->
    z_config:get(Key);
config_value(Key, Context) ->
    case m_site:get(Key, Context) of
        undefined -> z_config:get(Key);
        <<>> -> z_config:get(Key);
        "" -> z_config:get(Key);
        Value -> Value
    end.

normalize_optional_limit(undefined) ->
    undefined;
normalize_optional_limit(none) ->
    undefined;
normalize_optional_limit("none") ->
    undefined;
normalize_optional_limit(<<"none">>) ->
    undefined;
normalize_optional_limit(Value) ->
    case z_convert:to_integer(Value) of
        N when is_integer(N), N > 0 -> N;
        _ -> undefined
    end.
