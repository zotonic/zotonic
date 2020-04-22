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
    ok.
