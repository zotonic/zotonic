-module(z_gettext_plural_tests).
-include_lib("eunit/include/eunit.hrl").

language_rules_test() ->
    Cases = [
        {<<"nplurals=1; plural=0;">>, [{0,0}, {1,0}, {100,0}]},
        {<<"nplurals=2; plural=(n != 1);">>, [{0,1}, {1,0}, {2,1}]},
        {<<"nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;">>,
            [{0,2}, {1,0}, {2,1}, {4,1}, {5,2}]},
        {<<"nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;">>,
            [{1,0}, {2,1}, {5,2}, {11,2}, {21,0}, {22,1}, {111,2}]},
        {<<"nplurals=6; plural=n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 && n%100<=99 ? 4 : 5;">>,
            [{0,0}, {1,1}, {2,2}, {3,3}, {11,4}, {100,5}]}
    ],
    lists:foreach(fun({Header, Counts}) ->
        {ok, Rule} = z_gettext_plural:parse(Header),
        lists:foreach(fun({N, Index}) ->
            ?assertEqual({ok, Index}, template_compiler_plural:index(Rule, N))
        end, Counts)
    end, Cases).

expression_semantics_test() ->
    Cases = [
        {<<"n ? 2 : 1/0">>, 1, 2},
        {<<"n ? 1/0 : 2">>, 0, 2},
        {<<"n && 1/0">>, 0, 0},
        {<<"n || 1/0">>, 1, 1},
        {<<"1 + 2 * 3">>, 0, 7},
        {<<"8 / 3">>, 0, 2},
        {<<"8 - 3 - 2">>, 0, 3},
        {<<"!(n == 1)">>, 2, 1},
        {<<"-(-n) + +1">>, 2, 3},
        {<<"0 ? 1 : 0 ? 2 : 3">>, 0, 3}
    ],
    lists:foreach(fun({Expr, N, Expected}) ->
        {ok, Rule} = z_gettext_plural:parse(<<"nplurals=10; plural=", Expr/binary, ";">>),
        ?assertEqual({ok, Expected}, template_compiler_plural:index(Rule, N))
    end, Cases).

invalid_rules_test() ->
    lists:foreach(fun(Header) ->
        ?assertEqual({error, invalid_plural_forms}, z_gettext_plural:parse(Header))
    end, [<<>>, <<"nplurals=0; plural=0;">>, <<"nplurals=101; plural=0;">>,
        <<"nplurals=2; plural=foo;">>, <<"nplurals=2; plural=n();">>,
        <<"nplurals=2; plural=(n;">>, <<"nplurals=2; plural=n=1;">>,
        <<"nplurals=2; plural=n; trailing">>]),
    lists:foreach(fun(Expr) ->
        {ok, Rule} = z_gettext_plural:parse(<<"nplurals=2; plural=", Expr/binary, ";">>),
        ?assertEqual({error, invalid_plural_index}, template_compiler_plural:index(Rule, 5))
    end, [<<"-1">>, <<"2">>, <<"1/0">>]).
