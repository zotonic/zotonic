-module(z_props_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

props_test() ->
    {ok, Ps} = z_props:from_qs([
        {<<"dt:y:0:publication_start">>, <<"2008">>},
        {<<"dt:m:0:publication_start">>, <<"12">>},
        {<<"dt:d:0:publication_start">>, <<"10">>},
        {<<"dt:y:1:publication_end">>, <<"">>},
        {<<"dt:m:1:publication_end">>, <<"">>},
        {<<"dt:d:1:publication_end">>, <<"">>},
        {<<"plop">>, <<"hello">>}
    ]),
    #{
        <<"plop">> := <<"hello">>,
        <<"publication_end">> := ?ST_JUTTEMIS,
        <<"publication_start">> := {{2008, 12, 10}, {0, 0, 0}}
    } = Ps,
    %%
    {ok,#{<<"date_end">> := {{2009,12,31},{23,59,59}}}} = z_props:from_qs([{<<"dt:y:0:date_end">>,<<"2009">>}]),
    %%
    {ok,#{<<"date_start">> := {{-4700,1,1},{0,12,0}}}} = z_props:from_qs([{<<"dt:i:0:date_start">>,<<"12">>}]),
    %%
    {ok,#{<<"date_start">> := undefined}} = z_props:from_qs([{<<"dt:i:0:date_start">>,<<>>}]),
    %%
    {ok, #{<<"title">> := #trans{ tr = [
            {en, <<"Hello">>},
            {nl, <<"Hallo">>}
        ] }} } = z_props:from_qs([
                {<<"title$nl">>, <<" Hallo ">>},
                {<<"title$en">>, <<"Hello ">>}
            ]),
    %%
    {ok, #{ <<"a">> := #{ <<"b">> := #{ <<"c">> := 4 }, <<"d">> := 5 }}} =
        z_props:from_qs([
                {<<"a.b.c">>, 4},
                {<<"a.d">>, 5}
            ]),
    %%
    {ok, #{ <<"a">> := [ 1, 2, 3 ]}} =
        z_props:from_qs([
                {<<"a[]">>, 1},
                {<<"a[]">>, 2},
                {<<"a[]">>, 3}
            ]),
    %%
    {ok, #{ <<"a">> := [ #{ <<"b">> := 1, <<"c">> := 2 } ]}} =
        z_props:from_qs([
                {<<"a[].b">>, 1},
                {<<"a[].c">>, 2}
            ]),
    %%
    {ok, #{ <<"a">> := [ #{ <<"b">> := 1 }, #{ <<"c">> := 2 } ]}} =
        z_props:from_qs([
                {<<"a[].b">>, 1},
                {<<"a[].">>, <<>>},
                {<<"a[].c">>, 2}
            ]),
    %%
    {ok, #{
        <<"a">> := [
            #{ <<"b">> := #trans{ tr = [
                    {en, <<"Hello">> },
                    {nl, <<"Hallo">> }
                ]}
            },
            #{
                <<"c">> := 2
            }
        ]}} =
        z_props:from_qs([
                {<<"a[].b$nl">>, <<"Hallo">>},
                {<<"a[].b$en">>, <<"Hello">>},
                {<<"a[].">>, <<>>},
                {<<"a[].c">>, 2}
            ]),
    %%
    %%
    #{ <<"a">> := 1 }           = z_props:from_props([ {a, 1} ]),
    #{ <<"a">> := <<"hello">> } = z_props:from_props([ {a, "hello"} ]),
    #{ <<"a">> := [ 1, 256 ] }  = z_props:from_props([ {a, [ 1, 256 ]} ]),
    #{ <<"a">> := true }        = z_props:from_props([ a ]),
    %%
    #{
        <<"a">> := [
            #{ <<"b">> := true },
            #{ <<"c">> := false }
        ] } = z_props:from_props([ {a, [ [{b, true}], [{c, false }] ]} ]),
    %%
    %%
    [] = z_props:extract_languages( #{ <<"a">> => <<>> }),
    [ en ] = z_props:extract_languages( #{ <<"a">> => #trans{ tr = [ {en, <<>>} ] } }),
    [ en, nl ] = z_props:extract_languages(
            #{ <<"a">> => [
                    #trans{ tr = [ {en, <<>>} ] },
                    #{
                        <<"a">> => #trans{ tr = [ {nl, <<>>}, {en, <<>>} ] }
                    }
                ]
            }),
    #{ <<"a">> := #trans{ tr = [] } } = z_props:prune_languages( #{ <<"a">> => #trans{ tr = [ {en, <<>>} ] } }, [ nl] ),
    #{ <<"a">> := #trans{ tr = [ {en, <<>>} ] } } = z_props:prune_languages( #{ <<"a">> => #trans{ tr = [ {en, <<>>} ] } }, [ en, nl ] ),
    #{ <<"a">> := [
            #trans{ tr = [ {en, <<>>} ] },
            #{
                <<"a">> := #trans{ tr = [ {nl, <<>>}, {en, <<>>} ] }
            }
        ]
    } = z_props:prune_languages(
            #{ <<"a">> => [
                    #trans{ tr = [ {en, <<>>} ] },
                    #{
                        <<"a">> => #trans{ tr = [ {nl, <<>>}, {en, <<>>} ] }
                    }
                ]
            }, [ nl, en ]),
    #{ <<"a">> := [
            #trans{ tr = [] },
            #{
                <<"a">> := #trans{ tr = [ {nl, <<>>} ] }
            }
        ]
    } = z_props:prune_languages(
            #{ <<"a">> => [
                    #trans{ tr = [ {en, <<>>} ] },
                    #{
                        <<"a">> => #trans{ tr = [ {nl, <<>>}, {en, <<>>} ] }
                    }
                ]
            }, [ nl, de ]),

    % Property indices - appending and merging special cases
    {ok, #{
        <<"a">> := [ <<"a1">>, <<"b1">> ]
    }} = z_props:from_qs([ {<<"a[]">>, <<"a1">>}, {<<"a[]">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [ <<"a1">>, <<"b1">> ]
    }} = z_props:from_qs([ {<<"a[1]">>, <<"a1">>}, {<<"a[2]">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [ <<"b1">>, <<"a1">> ]
    }} = z_props:from_qs([ {<<"a[2]">>, <<"a1">>}, {<<"a[1]">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [ <<"b1">>, undefined, <<"a1">> ]
    }} = z_props:from_qs([ {<<"a[3]">>, <<"a1">>}, {<<"a[1]">>, <<"b1">>} ]),

    % Property indices - appending and merging special cases
    {ok, #{
        <<"a">> := [ #trans{ tr = [ {en, <<"a1">>}, {nl, <<"b1">>} ] } ]
    }} = z_props:from_qs([ {<<"a[1]$en">>, <<"a1">>}, {<<"a[1]$nl">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [
            #trans{ tr = [ {en, <<"a1">>} ] },
            #trans{ tr = [ {nl, <<"b1">>} ] }
        ]
    }} = z_props:from_qs([ {<<"a[1]$en">>, <<"a1">>}, {<<"a[2]$nl">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [ #trans{ tr = [ {en, <<"a1">>}, {nl, <<"b1">>} ] } ]
    }} = z_props:from_qs([ {<<"a[]$en">>, <<"a1">>}, {<<"a[]$nl">>, <<"b1">>} ]),
    {ok, #{
        <<"a">> := [
            #trans{ tr = [ {en, <<"a1">>} ] },
            #trans{ tr = [ {en, <<"b1">>} ] }
        ]
    }} = z_props:from_qs([ {<<"a[]$en">>, <<"a1">>}, {<<"a[]$en">>, <<"b1">>} ]),
    ok.

common_properties_test() ->
    Props = z_props:common_properties(),
    true = is_list(Props),
    true = length(Props) > 0,
    true = lists:all(fun is_binary/1, Props),
    true = lists:member(<<"title">>, Props),
    true = lists:member(<<"is_published">>, Props),
    true = lists:member(<<"body">>, Props),
    true = lists:member(<<"email">>, Props),
    ok.

property_name_type_hint_test() ->
    %% Key-specific matches
    id       = z_props:property_name_type_hint(<<"id">>),
    id       = z_props:property_name_type_hint(<<"creator_id">>),
    id       = z_props:property_name_type_hint(<<"modifier_id">>),
    id       = z_props:property_name_type_hint(<<"rsc_id">>),
    id       = z_props:property_name_type_hint(<<"rsc_id2">>),
    binary   = z_props:property_name_type_hint(<<"name">>),
    binary   = z_props:property_name_type_hint(<<"type">>),
    binary   = z_props:property_name_type_hint(<<"tz">>),
    int      = z_props:property_name_type_hint(<<"version">>),
    int      = z_props:property_name_type_hint(<<"visible_for">>),
    int      = z_props:property_name_type_hint(<<"privacy">>),
    datetime = z_props:property_name_type_hint(<<"created">>),
    datetime = z_props:property_name_type_hint(<<"modified">>),
    datetime = z_props:property_name_type_hint(<<"date_start">>),
    datetime = z_props:property_name_type_hint(<<"date_end">>),
    datetime = z_props:property_name_type_hint(<<"publication_start">>),
    datetime = z_props:property_name_type_hint(<<"publication_end">>),
    list     = z_props:property_name_type_hint(<<"language">>),
    list     = z_props:property_name_type_hint(<<"blocks">>),
    list     = z_props:property_name_type_hint(<<"is_a">>),
    language = z_props:property_name_type_hint(<<"pref_language">>),
    language = z_props:property_name_type_hint(<<"medium_language">>),
    float    = z_props:property_name_type_hint(<<"location_lat">>),
    float    = z_props:property_name_type_hint(<<"location_lng">>),
    uri      = z_props:property_name_type_hint(<<"@id">>),
    uri      = z_props:property_name_type_hint(<<"website">>),
    email    = z_props:property_name_type_hint(<<"email">>),
    html     = z_props:property_name_type_hint(<<"body">>),
    html     = z_props:property_name_type_hint(<<"body_extra">>),
    text     = z_props:property_name_type_hint(<<"title">>),
    text     = z_props:property_name_type_hint(<<"summary">>),
    text     = z_props:property_name_type_hint(<<"chapeau">>),
    text     = z_props:property_name_type_hint(<<"subtitle">>),
    %% is_/date_is_ prefix patterns -> bool
    bool     = z_props:property_name_type_hint(<<"is_published">>),
    bool     = z_props:property_name_type_hint(<<"is_featured">>),
    bool     = z_props:property_name_type_hint(<<"is_anything">>),
    bool     = z_props:property_name_type_hint(<<"date_is_all_day">>),
    bool     = z_props:property_name_type_hint(<<"seo_noindex">>),
    %% Suffix extraction fallback (last segment after final underscore)
    int      = z_props:property_name_type_hint(<<"foo_int">>),
    uri      = z_props:property_name_type_hint(<<"link_url">>),
    uri      = z_props:property_name_type_hint(<<"ref_uri">>),
    email    = z_props:property_name_type_hint(<<"contact_email">>),
    html     = z_props:property_name_type_hint(<<"intro_html">>),
    list     = z_props:property_name_type_hint(<<"items_list">>),
    id       = z_props:property_name_type_hint(<<"parent_id">>),
    datetime = z_props:property_name_type_hint(<<"expiry_date">>),
    unsafe   = z_props:property_name_type_hint(<<"raw_unsafe">>),
    %% property_name_type_hint_1 prefix patterns
    email    = z_props:property_name_type_hint(<<"email_from">>),
    datetime = z_props:property_name_type_hint(<<"date_next">>),
    binary   = z_props:property_name_type_hint(<<"address_city">>),
    binary   = z_props:property_name_type_hint(<<"mail_street_1">>),
    binary   = z_props:property_name_type_hint(<<"billing_country">>),
    %% Unknown key -> undefined
    undefined = z_props:property_name_type_hint(<<"unknown_key">>),
    undefined = z_props:property_name_type_hint(<<"foobar">>),
    ok.
