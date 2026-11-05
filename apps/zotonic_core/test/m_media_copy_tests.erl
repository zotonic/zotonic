%% @doc Copying a medium onto an existing resource, including CAFS ownership.
-module(m_media_copy_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

copy_files_test_() -> {timeout,60,fun copy_files/0}.
copy_files() ->
    C = context(),
    {ok, Source} = upload(C),
    {ok, Destination} = upload(C),
    Image = filename:join(code:priv_dir(zotonic_site_testsandbox),"files/archive/koe.jpg"),
    {ok, ImageData} = file:read_file(Image),
    {ok, _} = m_media:save_preview(Source,ImageData,<<"image/jpeg">>,C),
    Before = m_media:get(Destination,C),
    Src = m_media:get(Source,C),
    #{<<"filename">> := SourceFile, <<"preview_filename">> := SourcePreview, <<"digest">> := Hash} = Src,
    {ok, PreviewHash} = m_cafs:lookup(SourcePreview,C),
    Physical = z_path:files_subdir(m_cafs:path(Hash),C),
    {ok, Bytes} = file:read_file(Physical),
    try
        {ok,Destination} = m_rsc:update(Destination,#{<<"title">> => <<"Keep destination title">>},C),
        Category = m_rsc:p(Destination,category_id,C),
        %% A mapped remote-only original is copied without fetching its bytes.
        ok = file:delete(Physical),
        ok = m_media:copy(Source,Destination,C),
        Copy = m_media:get(Destination,C),
        #{<<"filename">> := CopyFile, <<"preview_filename">> := CopyPreview} = Copy,
        ?assertNotEqual(SourceFile,CopyFile),
        ?assertNotEqual(SourcePreview,CopyPreview),
        ?assertEqual({ok,Hash},m_cafs:lookup(CopyFile,C)),
        ?assertEqual({ok,PreviewHash},m_cafs:lookup(CopyPreview,C)),
        ?assertEqual(Src,m_media:get(Source,C)),
        ?assertEqual(Destination,maps:get(<<"id">>,Copy)),
        ?assertEqual(Hash,maps:get(<<"digest">>,Copy)),
        ?assertEqual(Category,m_rsc:p(Destination,category_id,C)),
        ?assertEqual(<<"Keep destination title">>,m_rsc:p(Destination,title,C)),
        ?assertNotEqual(undefined,z_db:q1("select retired_at from filename_cafs where filename=$1",
            [maps:get(<<"filename">>,Before)],C)),
        %% Same-resource copy neither changes the record nor allocates aliases.
        Count = alias_count(Hash,C),
        ok = m_media:copy(Source,Source,C),
        ?assertEqual(Src,m_media:get(Source,C)),
        ?assertEqual(Count,alias_count(Hash,C)),
        ok = m_rsc:delete(Source,C),
        ?assertEqual({ok,Hash},m_cafs:lookup(CopyFile,C)),
        ?assertEqual({ok,PreviewHash},m_cafs:lookup(CopyPreview,C))
    after
        file:write_file(Physical,Bytes),
        m_rsc:delete(Source,C),m_rsc:delete(Destination,C)
    end.

copy_failure_test_() -> {timeout,60,fun copy_failure/0}.
copy_failure() ->
    C = context(),
    {ok, Source} = upload(C),
    {ok, Destination} = upload(C),
    {ok, Empty} = m_rsc:insert(#{<<"category">> => media},C),
    try
        Before = m_media:get(Destination,C),
        ?assertEqual({error,enoent},m_media:copy(Empty,Destination,C)),
        ?assertEqual({error,enoent},m_media:copy(<<"missing-copy-source">>,Destination,C)),
        ?assertEqual({error,enoent},m_media:copy(Source,<<"missing-copy-target">>,C)),
        ?assertEqual({error,eacces},m_media:copy(Source,Destination,z_context:new(zotonic_site_testsandbox))),
        Src = m_media:get(Source,C),
        Hash = maps:get(<<"digest">>,Src),
        ok = m_media:replace(Source,Src#{
            <<"preview_filename">> => <<"missing/",(z_ids:id())/binary,".jpg">>,
            <<"is_deletable_preview">> => true
        },C),
        Count = alias_count(Hash,C),
        ?assertMatch({error,_},m_media:copy(Source,Destination,C)),
        ?assertEqual(Count,alias_count(Hash,C)),
        ?assertEqual(Before,m_media:get(Destination,C))
    after m_rsc:delete(Source,C),m_rsc:delete(Destination,C),m_rsc:delete(Empty,C) end.

copy_embed_test_() -> {timeout,60,fun copy_embed/0}.
copy_embed() ->
    C = context(),
    Name = <<"copy-source-",(z_ids:id())/binary>>,
    {ok, Source} = m_rsc:insert(#{<<"category">> => media, <<"name">> => Name},C),
    {ok, Destination} = m_rsc:insert(#{<<"category">> => media},C),
    try
        ok = m_media:replace(Source,#{<<"mime">> => <<"text/html-video-embed">>,
            <<"video_embed_code">> => <<"<iframe></iframe>">>},C),
        ok = m_media:copy(Name,Destination,C),
        ?assertEqual(<<"<iframe></iframe>">>,maps:get(<<"video_embed_code">>,m_media:get(Destination,C))),
        ?assertEqual(Destination,maps:get(<<"id">>,m_media:get(Destination,C)))
    after m_rsc:delete(Source,C),m_rsc:delete(Destination,C) end.

legacy_files_test_() ->
    [{timeout,60,fun() -> legacy_files(Owned,Operation) end}
        || Owned <- [true,false], Operation <- [copy,duplicate]].

legacy_files(Owned, Operation) ->
    C = context(),
    Filename = <<"copy-legacy/",(z_ids:id())/binary,".txt">>,
    Preview = <<"copy-legacy/",(z_ids:id())/binary,".jpg">>,
    File = z_media_archive:abspath(Filename,C),
    PreviewFile = z_media_archive:abspath(Preview,C),
    Bytes = <<"legacy original ",(z_ids:id())/binary>>,
    PreviewBytes = <<"legacy preview ",(z_ids:id())/binary>>,
    ok = z_filelib:ensure_dir(File),
    ok = file:write_file(File,Bytes),
    ok = file:write_file(PreviewFile,PreviewBytes),
    {ok,Source} = m_rsc:insert(#{<<"category">> => media},C),
    {ok,Destination} = m_rsc:insert(#{<<"category">> => media},C),
    try
        ok = m_media:replace(Source,#{<<"filename">> => Filename,
            <<"preview_filename">> => Preview, <<"mime">> => <<"text/plain">>,
            <<"is_deletable_file">> => Owned, <<"is_deletable_preview">> => Owned},C),
        ?assertEqual({error,enoent},m_cafs:lookup(Filename,C)),
        ?assertEqual({error,enoent},m_cafs:lookup(Preview,C)),
        ok = apply(m_media,Operation,[Source,Destination,C]),
        Copied = m_media:get(Destination,C),
        CopyFile = maps:get(<<"filename">>,Copied),
        CopyPreview = maps:get(<<"preview_filename">>,Copied),
        {ok,Hash} = m_cafs:lookup(CopyFile,C),
        {ok,PreviewHash} = m_cafs:lookup(CopyPreview,C),
        ?assertEqual(Hash,maps:get(<<"digest">>,Copied)),
        ?assertEqual(true,maps:get(<<"is_deletable_file">>,Copied)),
        ?assertEqual(true,maps:get(<<"is_deletable_preview">>,Copied)),
        ?assertNotEqual(Filename,CopyFile),
        ?assertNotEqual(Preview,CopyPreview),
        ?assertEqual({ok,Bytes},m_cafs:with_local_file(Hash,fun file:read_file/1,C)),
        ?assertEqual({ok,PreviewBytes},m_cafs:with_local_file(PreviewHash,fun file:read_file/1,C)),
        ExpectedSource = case Owned of true -> {ok,Hash}; false -> {error,enoent} end,
        ?assertEqual(ExpectedSource,m_cafs:lookup(Filename,C)),
        ?assertEqual({ok,Bytes},file:read_file(File)),
        ?assertEqual({ok,PreviewBytes},file:read_file(PreviewFile))
    after
        m_rsc:delete(Source,C),m_rsc:delete(Destination,C),
        file:delete(File),file:delete(PreviewFile)
    end.

library_preview_test_() -> {timeout,60,fun library_preview/0}.
library_preview() ->
    C = context(),
    {ok,Source} = m_rsc:insert(#{<<"category">> => media},C),
    {ok,Destination} = m_rsc:insert(#{<<"category">> => media},C),
    Preview = <<"lib/images/placeholder.png">>,
    try
        ok = m_media:replace(Source,#{<<"mime">> => <<"text/html-video-embed">>,
            <<"preview_filename">> => Preview, <<"is_deletable_preview">> => false},C),
        ok = m_media:copy(Source,Destination,C),
        Copied = m_media:get(Destination,C),
        ?assertMatch({ok,_},m_cafs:lookup(maps:get(<<"preview_filename">>,Copied),C)),
        ?assertEqual(true,maps:get(<<"is_deletable_preview">>,Copied)),
        ?assertEqual({error,enoent},m_cafs:lookup(Preview,C)),
        ?assertEqual(Preview,maps:get(<<"preview_filename">>,m_media:get(Source,C)))
    after m_rsc:delete(Source,C),m_rsc:delete(Destination,C) end.

context() -> z_acl:sudo(z_context:new(zotonic_site_testsandbox)).
upload(C) -> m_media:insert_file(#upload{filename= <<"copy.txt">>,
    data= <<"Copy test ",(z_ids:id())/binary>>},C).
alias_count(Hash,C) -> z_db:q1("select count(*) from filename_cafs where hash=$1",[Hash],C).
