-module(z_media_upload_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

reuse_test_() -> {timeout, 60, fun reuse/0}.
reuse() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Bytes = z_ids:id(),
    {ok, Id} = m_media:insert_file(#upload{filename= <<"original.txt">>, data=Bytes},
        #{<<"is_published">> => false}, [], C),
    #{<<"filename">> := Name} = m_media:get(Id, C),
    {ok, Hash} = m_cafs:lookup(Name, C),
    Payload = #{<<"filename">> => <<"new-name.txt">>, <<"size">> => byte_size(Bytes), <<"digest">> => Hash},
    try
        {ok, #{needed := false, media_upload_token := Token}} = z_media_upload:check(Payload, C),
        Value = #{<<"media_upload_token">> => Token},
        {ok, #upload{filename= <<"new-name.txt">>, tmpfile=File}} = z_media_upload:materialize(Value, C),
        ?assertEqual({ok, Bytes}, file:read_file(File)),
        file:delete(File),
        ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload#{<<"size">> => 1}, C)),
        ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload#{<<"digest">> => binary:copy(<<"0">>,64)}, C)),
        ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload#{<<"digest">> => <<"bad">>}, C)),
        ?assertEqual({error, eacces}, z_media_upload:materialize(
            #{<<"media_upload_token">> => <<"invalid">>}, C)),
        {media_upload, User, _, Id, Name, Hash, Size, Filename} = z_crypto:depickle(Token, C),
        Expired = z_crypto:pickle({media_upload, User, 0, Id, Name, Hash, Size, Filename}, C),
        ?assertEqual({error,eacces}, z_media_upload:materialize(#{<<"media_upload_token">> => Expired}, C)),
        OtherUser = z_crypto:pickle({media_upload, -123, erlang:system_time(second)+3600,
            Id, Name, Hash, Size, Filename}, C),
        ?assertEqual({error,eacces}, z_media_upload:materialize(#{<<"media_upload_token">> => OtherUser}, C)),
        %% Matching private bytes must be indistinguishable from a miss, even
        %% when upload permissions themselves allow insertion.
        meck:new(z_acl, [passthrough, no_link]),
        try
            meck:expect(z_acl, rsc_visible, fun(_, _) -> false end),
            ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload, C)),
            ?assertEqual({error,eacces}, z_media_upload:materialize(Value, C))
        after meck:unload(z_acl) end,
        {ok, [{<<"file">>, #upload{} = Upload}]} = z_media_upload:validate_args([{<<"file">>, Value}], C),
        {ok, Copy} = m_media:insert_file(Upload, #{}, [], C),
        try
            #{<<"filename">> := CopyName} = m_media:get(Copy, C),
            ?assertEqual({ok, Hash}, m_cafs:lookup(CopyName, C))
        after m_rsc:delete(Copy, C), file:delete(Upload#upload.tmpfile) end,
        %% A source deleted after preflight cannot authorize a later submission.
        ok = m_media:delete(Id, C),
        ?assertEqual({error,eacces}, z_media_upload:materialize(Value, C))
    after m_rsc:delete(Id, C) end.

legacy_test_() -> {timeout, 60, fun legacy/0}.
legacy() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Bytes = z_ids:id(),
    Hash = z_crypto:hex_sha2(Bytes),
    Name = <<"upload-test/", Bytes/binary, ".txt">>,
    File = z_media_archive:abspath(Name, C),
    ok = z_filelib:ensure_dir(File),
    ok = file:write_file(File, Bytes),
    {ok, Id} = m_rsc:insert(#{category => media, title => <<"Legacy preflight test">>}, C),
    {ok, _} = z_db:insert(medium, #{id => Id, filename => Name,
        size => byte_size(Bytes), is_deletable_file => true}, C),
    Payload = #{<<"filename">> => <<"copy.txt">>, <<"size">> => byte_size(Bytes), <<"digest">> => Hash},
    try
        %% No checksum: do not scan old files looking for matching bytes.
        ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload, C)),
        {ok, _} = z_db:update(medium, Id, #{digest => Hash}, C),
        meck:new(z_acl, [passthrough, no_link]),
        try
            meck:expect(z_acl, rsc_visible, fun(_, _) -> false end),
            ?assertEqual({ok, #{needed => true}}, z_media_upload:check(Payload, C)),
            ?assertEqual({error, enoent}, m_cafs:lookup(Name, C))
        after meck:unload(z_acl) end,
        ?assertMatch({ok, #{needed := false}}, z_media_upload:check(Payload, C)),
        ?assertEqual({ok, Hash}, m_cafs:lookup(Name, C))
    after
        m_rsc:delete(Id, C),
        file:delete(File)
    end.
