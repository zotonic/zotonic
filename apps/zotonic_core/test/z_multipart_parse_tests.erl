%% @hidden

-module(z_multipart_parse_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


boundary_length_rejected_test() ->
    Config = z_multipart_parse:test_config(#{
        max_boundary_length => 3
    }),
    ?assertThrow(
        {stop_request, 400},
        z_multipart_parse:test_get_boundary(
            <<"multipart/form-data; boundary=abcd">>,
            Config)).


max_content_length_streaming_enforced_test() ->
    Config = z_multipart_parse:test_config(#{
        max_content_length => 10
    }),
    ok = meck:new(cowmachine_req, [passthrough, no_link]),
    ok = meck:expect(cowmachine_req, stream_req_body,
        fun(_ChunkSize, undefined) ->
            {more, <<"xx">>, undefined}
        end),
    try
        ?assertThrow(
            {stop_request, 413},
            z_multipart_parse:test_read_more(Config, 100, 9))
    after
        meck:unload(cowmachine_req)
    end.


max_fields_counting_test() ->
    Config = z_multipart_parse:test_config(#{
        max_fields => 1
    }),
    Headers = [
        {<<"content-disposition">>, {<<"form-data">>, [
            {<<"name">>, <<"title">>}
        ]}}
    ],
    ?assertThrow(
        {stop_request, 413},
        z_multipart_parse:test_check_part_headers(
            Headers,
            Config,
            #{field_count => 1})).


max_files_counting_test() ->
    Config = z_multipart_parse:test_config(#{
        max_files => 1
    }),
    Headers = [
        {<<"content-disposition">>, {<<"form-data">>, [
            {<<"name">>, <<"upload">>},
            {<<"filename">>, <<"file.bin">>}
        ]}}
    ],
    ?assertThrow(
        {stop_request, 413},
        z_multipart_parse:test_check_part_headers(
            Headers,
            Config,
            #{file_count => 1})).


missing_form_data_name_rejected_test() ->
    Config = z_multipart_parse:test_config(#{}),
    Headers = [
        {<<"content-disposition">>, {<<"form-data">>, [
            {<<"filename">>, <<"file.bin">>}
        ]}}
    ],
    ?assertThrow(
        {stop_request, 400},
        z_multipart_parse:test_check_part_headers(
            Headers,
            Config,
            #{})).


acl_max_upload_size_override_anonymous_context_test() ->
    AclMaxUploadSize = 20 * 1024 * 1024,
    Config = z_multipart_parse:test_config(#{
        max_content_length => 100 * 1024 * 1024,
        max_file_length => 10 * 1024 * 1024,
        max_files => 7
    }),
    Context = #context{user_id = undefined},
    ok = meck:new(z_notifier, [passthrough, no_link]),
    ok = meck:expect(z_notifier, first,
        fun(#acl_max_upload_size{}, ContextArg) when ContextArg =:= Context ->
            AclMaxUploadSize
        end),
    try
        Config1 = z_multipart_parse:test_acl_upload_size_config(Context, Config),
        ?assertEqual(AclMaxUploadSize, maps:get(max_file_length, Config1)),
        ?assertEqual(7, maps:get(max_files, Config1)),
        ?assert(maps:get(max_content_length, Config1) > AclMaxUploadSize)
    after
        meck:unload(z_notifier)
    end.

-endif.
