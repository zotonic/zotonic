%% @doc Mandatory background read on language tags: [1].
%%
%%      Some quotes from [1]:
%%
%%          The golden rule when creating language tags is to keep the tag as short as
%%          possible. Avoid region, script or other subtags except where they add useful
%%          distinguishing information. For instance, use 'ja' for Japanese and not
%%          'ja-JP', unless there is a particular reason that you need to say that this is
%%          Japanese as spoken in Japan, rather than elsewhere.
%%
%%          The entries in the registry follow certain conventions with regard to upper
%%          and lower letter-casing. For example, language tags are lower case, alphabetic
%%          region subtags are upper case, and script tags begin with an initial capital.
%%          This is only a convention!
%%
%%      Note that we use lower case subtags in subtag identifiers and URLs.
%%
%%          Language+extlang combinations are provided to accommodate legacy language tag
%%          forms, however, there is a single language subtag available for every
%%          language+extlang combination. That language subtag should be used rather than
%%          the language+extlang combination, where possible. For example, use 'yue'
%%          rather than 'zh-yue' for Cantonese, and 'afb' rather than 'ar-afb' for Gulf
%%          Arabic, if you can.

%%      Language identifiers can have the following forms:
%%      - language;
%%      - language-extlang;
%%      - language-region;
%%      - language-script;
%%      It is discouraged to use language-script-region, but it is possible if
%%      required.
%%      For a list of language, region and script codes, see [2].
%%      [1] http://www.w3.org/International/articles/language-tags/
%%      [2] http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
-module(z_language_data).

-export([
        is_language/1,
        to_language_atom/1,
        fallback/1,
        codes_bin/0,
        codes_atom/0,

        language_names_en/0,

        languages_map_flat/0,
        languages_map_main/0,
        languages_list/0,

        make_mod/0,
        make_mod_reload/0
    ]).

-define(MODULE_MAP, 'z_language_data$map').

-spec is_language( z_language:language() ) -> boolean().
is_language( Lang ) when is_atom(Lang); is_binary(Lang) ->
    maps:is_key(Lang, languages_map_flat());
is_language( Lang ) when is_list(Lang) ->
    is_language(z_convert:to_binary(Lang)).

-spec to_language_atom( z_language:language() ) -> {ok, atom()} | {error, not_a_language}.
to_language_atom( Lang ) when is_atom(Lang); is_binary(Lang) ->
    case maps:find(Lang, languages_map_flat()) of
        {ok, #{ code_atom := Code }} -> {ok, Code};
        error ->
            Lower = z_string:to_lower(z_convert:to_binary(Lang)),
            case maps:find(Lower, languages_map_flat()) of
                {ok, #{ code_atom := Code }} -> {ok, Code};
                error -> {error, not_a_language}
            end
    end;
to_language_atom( Lang ) when is_list(Lang) ->
    to_language_atom(z_convert:to_binary(Lang)).


-spec fallback(  z_language:language() ) -> list( z_language:language_code() ).
fallback(Code) when is_atom(Code); is_binary(Code) ->
    try
        Fallback = erlang:apply(?MODULE_MAP, fallback, []),
        maps:get(Code, Fallback, [])
    catch _:_ ->
        make_mod(),
        fallback(Code)
    end;
fallback(Code) when is_list(Code) ->
    fallback(z_convert:to_binary(Code)).

-spec language_names_en() -> [ LanguageName ] when
    LanguageName :: binary().
language_names_en() ->
    All = languages_map_flat(),
    Names = maps:fold(
        fun(_Key, #{ name_en := Name }, Acc) ->
            [ Name | Acc ]
        end,
        [],
        All),
    lists:usort(Names).


-spec languages_map_flat() -> #{ (z_language:language_code() | binary()) => map() }.
languages_map_flat() ->
    try erlang:apply(?MODULE_MAP, languages_map_flat, [])
    catch _:_ -> make_mod(), languages_map_flat()
    end.

languages_map_main() ->
    try erlang:apply(?MODULE_MAP, languages_map_main, [])
    catch _:_ -> make_mod(), languages_map_main()
    end.


-spec codes_bin() -> list( binary() ).
codes_bin() ->
    try erlang:apply(?MODULE_MAP, codes_bin, [])
    catch _:_ -> make_mod(), codes_bin()
    end.

-spec codes_atom() -> list( atom() ).
codes_atom() ->
    try erlang:apply(?MODULE_MAP, codes_atom, [])
    catch _:_ -> make_mod(), codes_atom()
    end.

%% Generate and memorize a quick lookup map for language data.
make_mod() ->
    jobs:run(zotonic_singular_job, fun make_mod_1/0).

make_mod_1() ->
    case code:is_loaded(?MODULE_MAP) of
        false -> make_mod_reload();
        {file, _} -> ok
    end.

make_mod_reload() ->
    Ls = [ K || {K, _} <- languages_list() ],
    LsA = [ binary_to_atom(K,utf8) || K <- Ls ],
    Fallback = fetch_fallbacks( languages_list() ),
    Bin = compile(?MODULE_MAP, Ls, LsA, Fallback),
    code:purge(?MODULE_MAP),
    Filename = z_convert:to_list(?MODULE_MAP) ++ ".erl",
    code:load_binary(?MODULE_MAP, Filename, Bin).

fetch_fallbacks(List) ->
    fetch_fallback(List, [], #{}).

fetch_fallback([], _Path, Acc) ->
    Acc;
fetch_fallback([ {CodeBin, Props} | List ], Path0, Acc) ->
    Path = case proplists:get_value(fallback, Props) of
        undefined -> Path0;
        Fallback -> [ z_convert:to_atom(Fallback) | Path0 ]
    end,
    CodeAtom = z_convert:to_atom(CodeBin),
    Acc1 = Acc#{
        CodeBin => Path,
        CodeAtom => Path
    },
    Sub = proplists:get_value(sublanguages, Props, []),
    Acc2 = fetch_fallback(Sub, [ CodeAtom | Path ], Acc1),
    Acc3 = lists:foldl(
        fun(Alias, AliasAcc) ->
            AAtom = binary_to_atom(Alias, utf8),
            AliasAcc#{
                AAtom => Path,
                Alias => Path
            }
        end,
        Acc2,
        proplists:get_value(alias, Props, [])),
    fetch_fallback(List, Path0, Acc3).

as_map_flat(Fallback) ->
    as_map_flat(languages_list(), Fallback, #{}).

as_map_flat(List, Fallback, Map) ->
    lists:foldl(
        fun({Code, Props}, Acc) ->
            CodeAtom = binary_to_atom(Code, utf8),
            MapProps = as_map_flat_props(CodeAtom, Props, Fallback),
            Acc1 = Acc#{
                CodeAtom => MapProps,
                Code => MapProps
            },
            Acc2 = lists:foldl(
                fun(Alias, AliasAcc) ->
                    AAtom = binary_to_atom(Alias, utf8),
                    AliasAcc#{
                        AAtom => MapProps,
                        Alias => MapProps
                    }
                end,
                Acc1,
                proplists:get_value(alias, Props, [])),
            as_map_flat(proplists:get_value(sublanguages, Props, []), Fallback, Acc2)
        end,
        Map,
        List).

as_map_flat_props(CodeAtom, Props, Fallback) ->
    M = maps:from_list(Props),
    M#{
        code_atom => CodeAtom,
        code_bin => atom_to_binary(CodeAtom, utf8),
        language_atom => binary_to_atom( maps:get(language, M), utf8 ),
        sublanguages => as_atom_map(maps:get(sublanguages, M, []), Fallback, #{}),
        fallback => maps:get(CodeAtom, Fallback),
        sort_key => z_string:to_lower( proplists:get_value(name_en, Props) )
    }.

as_atom_map(Fallback) ->
    as_atom_map(languages_list(), Fallback, #{}).

as_atom_map(List, Fallback, Map) ->
    lists:foldl(
        fun({Code, Props}, Acc) ->
            CodeAtom = binary_to_atom(Code, utf8),
            MapProps = as_atom_map_props(CodeAtom, Props, Fallback),
            Acc#{
                CodeAtom => MapProps
            }
        end,
        Map,
        List).

as_atom_map_props(CodeAtom, Props, Fallback) ->
    M = maps:from_list(Props),
    M#{
        language_atom => binary_to_atom( maps:get(language, M), utf8 ),
        sublanguages => as_atom_map(maps:get(sublanguages, M, []), Fallback, #{}),
        fallback => maps:get(CodeAtom, Fallback),
        sort_key => z_string:to_lower( proplists:get_value(name_en, Props) )
    }.

-spec compile(atom(), list(), list(), map()) -> binary().
compile(Module, Ls, LsA, Fallback) ->
    case compile:forms(forms(Module, Ls, LsA, Fallback), [verbose, report_errors]) of
        {ok, _Module, Bin} when is_binary(Bin) ->
            Bin
    end.

-spec forms(atom(), list(), list(), map()) -> [erl_syntax:syntaxTree()].
forms(Module, Ls, LsA, Fallback) ->
    [ erl_syntax:revert(X) || X <- term_to_abstract(Module, Ls, LsA, Fallback)].

-spec term_to_abstract(atom(), list(), list(), map()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Ls, LsA, Fallback) ->
[
    % -module(Module).
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),

    % -export([ ... ]).
    erl_syntax:attribute(
        erl_syntax:atom(export),
        [
            erl_syntax:list([
                erl_syntax:arity_qualifier(erl_syntax:atom(languages_map_flat), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(languages_map_main), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(fallback), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(codes_bin), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(codes_atom), erl_syntax:integer(0))
            ])
        ]),

    % Functions
    erl_syntax:function(
        erl_syntax:atom(languages_map_flat),
        [ erl_syntax:clause([], none, [ erl_syntax:abstract( as_map_flat(Fallback) )]) ]),
    erl_syntax:function(
        erl_syntax:atom(languages_map_main),
        [ erl_syntax:clause([], none, [erl_syntax:abstract( as_atom_map(Fallback) )]) ]),
    erl_syntax:function(
        erl_syntax:atom(fallback),
        [ erl_syntax:clause([], none, [erl_syntax:abstract(Fallback)]) ]),
    erl_syntax:function(
        erl_syntax:atom(codes_bin),
        [ erl_syntax:clause([], none, [erl_syntax:abstract(Ls)]) ]),
    erl_syntax:function(
        erl_syntax:atom(codes_atom),
        [ erl_syntax:clause([], none, [erl_syntax:abstract(LsA)]) ])
].


%% ================================================================================================================
%% ================================================================================================================
%% ================================================================================================================

-spec languages_list() -> list( {binary(), proplists:proplist()} ).
languages_list() -> [
    {<<"aa">>, [
        {language, <<"aa">>},
        {name, <<"Qafaraf"/utf8>>},
        {name_en, <<"Afar"/utf8>>}
    ]},
    {<<"ab">>, [
        {language, <<"ab">>},
        {script, <<"Cyrl">>},
        {region, <<"GE">>},
        {name, <<"Аҧсуа бызшәа"/utf8>>},
        {name_en, <<"Abkhazian"/utf8>>}
    ]},
    {<<"af">>, [
        {language, <<"af">>},
        {name, <<"Afrikaans"/utf8>>},
        {name_en, <<"Afrikaans"/utf8>>}
    ]},
    {<<"am">>, [
        {language, <<"am">>},
        {script, <<"Ethi">>},
        {region, <<"ET">>},
        {name, <<"አማርኛ"/utf8>>},
        {name_en, <<"Amharic"/utf8>>}
    ]},
    {<<"ar">>, [
        {type, <<"macro_language">>},
        {language, <<"ar">>},
        {direction, <<"RTL">>},
        {script, <<"Arab">>},
        {name, <<"العربية"/utf8>>},
        {name_en, <<"Arabic"/utf8>>},
        {sublanguages, [
            {<<"arb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"اللغة العربية الفصحى"/utf8>>},
                {name_en, <<"Standard Arabic"/utf8>>}
            ]},
            {<<"afb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الخليج"/utf8>>},
                {name_en, <<"Arabic - Gulf"/utf8>>}
            ]},
            {<<"ajp">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - جنوب بلاد الشام"/utf8>>},
                {name_en, <<"Arabic - South Levant"/utf8>>}
            ]},
            {<<"apc">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - شمال بلاد الشام"/utf8>>},
                {name_en, <<"Arabic - North Levant"/utf8>>}
            ]},
            {<<"apd">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - السودان"/utf8>>},
                {name_en, <<"Arabic - Sudan"/utf8>>}
            ]},
            {<<"ar-ae">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"AE">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الإمارات العربية المتحدة."/utf8>>},
                {name_en, <<"Arabic - U.A.E."/utf8>>}
            ]},
            {<<"ar-bh">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"BH">>},
                {script, <<"Arab">>},
                {name, <<"العربية - البحرين"/utf8>>},
                {name_en, <<"Arabic - Bahrain"/utf8>>}
            ]},
            {<<"aao">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"DZ">>},
                {script, <<"Arab">>},
                {name, <<"العربية الصحراء الجزائرية"/utf8>>},
                {name_en, <<"Arabic - Algerian Sahara"/utf8>>}
            ]},
            {<<"ary">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"DZ">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المغرب"/utf8>>},
                {name_en, <<"Arabic - Marocco"/utf8>>}
            ]},
            {<<"arz">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"EG">>},
                {script, <<"Arab">>},
                {name, <<"مصر"/utf8>>},
                {name_en, <<"Arabic - Egypt"/utf8>>}
            ]},
            {<<"ar-iq">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"IQ">>},
                {script, <<"Arab">>},
                {name, <<"العربية - مصر"/utf8>>},
                {name_en, <<"Arabic - Iraq"/utf8>>}
            ]},
            {<<"ar-jo">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"JO">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الأردن"/utf8>>},
                {name_en, <<"Arabic - Jordan"/utf8>>}
            ]},
            {<<"ar-kw">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"KW">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الكويت"/utf8>>},
                {name_en, <<"Arabic - Kuwait"/utf8>>}
            ]},
            {<<"ar-lb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"LB">>},
                {script, <<"Arab">>},
                {name, <<"العربية - لبنان"/utf8>>},
                {name_en, <<"Arabic - Lebanon"/utf8>>}
            ]},
            {<<"ayl">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"LY">>},
                {script, <<"Arab">>},
                {name, <<"العربية - ليبيا"/utf8>>},
                {name_en, <<"Arabic - Libya"/utf8>>}
            ]},
            {<<"ar-ma">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"MA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المغرب"/utf8>>},
                {name_en, <<"Arabic - Morocco"/utf8>>}
            ]},
            {<<"acx">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"OM">>},
                {script, <<"Arab">>},
                {name, <<"العربية - عمان"/utf8>>},
                {name_en, <<"Arabic - Oman"/utf8>>}
            ]},
            {<<"ar-qa">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"QA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - قطر"/utf8>>},
                {name_en, <<"Arabic - Qatar"/utf8>>}
            ]},
            {<<"ar-sa">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"SA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المملكة العربية السعودية"/utf8>>},
                {name_en, <<"Arabic - Saudi Arabia"/utf8>>}
            ]},
            {<<"ar-sy">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"SY">>},
                {script, <<"Arab">>},
                {name, <<"العربية - سوريا"/utf8>>},
                {name_en, <<"Arabic - Syria"/utf8>>}
            ]},
            {<<"aeb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"TN">>},
                {script, <<"Arab">>},
                {name, <<"العربية - تونس"/utf8>>},
                {name_en, <<"Arabic - Tunisia"/utf8>>}
            ]},
            {<<"ar-ye">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"YE">>},
                {script, <<"Arab">>},
                {name, <<"العربية - اليمن"/utf8>>},
                {name_en, <<"Arabic - Yemen"/utf8>>}
            ]}
        ]}
    ]},
    {<<"as">>, [
        {language, <<"as">>},
        {script, <<"Beng">>},
        {name, <<"অসমীয়া"/utf8>>},
        {name_en, <<"Assamese"/utf8>>}
    ]},
    {<<"ay">>, [
        {language, <<"ay">>},
        {name, <<"Aymar aru"/utf8>>},
        {name_en, <<"Aymara"/utf8>>}
    ]},
    {<<"az">>, [
        {type, <<"macrolanguage">>},
        {language, <<"az">>},
        {name, <<"Azərbaycan dili"/utf8>>},
        {name_en, <<"Azerbaijani"/utf8>>}
    ]},
    {<<"ba">>, [
        {language, <<"ba">>},
        {script, <<"Cyrl">>},
        {region, <<"RU">>},
        {name, <<"башҡорт теле"/utf8>>},
        {name_en, <<"Bashkir"/utf8>>}
    ]},
    {<<"be">>, [
        {language, <<"be">>},
        {script, <<"Cyrl">>},
        {region, <<"BY">>},
        {name, <<"беларуская"/utf8>>},
        {name_en, <<"Byelorussian"/utf8>>}
    ]},
    {<<"bg">>, [
        {language, <<"bg">>},
        {script, <<"Cyrl">>},
        {region, <<"BG">>},
        {name, <<"български"/utf8>>},
        {name_en, <<"Bulgarian"/utf8>>}
    ]},
    %% Omitting "bh", which is "Bihari languages", a collection
    {<<"bi">>, [
        {language, <<"bi">>},
        {region, <<"VU">>},
        {name, <<"Bislama"/utf8>>},
        {name_en, <<"Bislama"/utf8>>}
    ]},
    {<<"bn">>, [
        {language, <<"bn">>},
        {script, <<"Beng">>},
        {name, <<"বাংলা"/utf8>>},
        {name_en, <<"Bengali"/utf8>>}
    ]},
    {<<"bo">>, [
        {language, <<"bo">>},
        {script, <<"Tibt">>},
        {name, <<"བོད་སྐད"/utf8>>},
        {name_en, <<"Tibetan"/utf8>>}
    ]},
    {<<"br">>, [
        {language, <<"br">>},
        {name, <<"Brezhoneg"/utf8>>},
        {name_en, <<"Breton"/utf8>>}
    ]},
    {<<"bs">>, [
        {language, <<"bs">>},
        {region, <<"BA">>},
        {name, <<"Bosanski"/utf8>>},
        {name_en, <<"Bosnian"/utf8>>}
    ]},
    {<<"ca">>, [
        {language, <<"ca">>},
        {region, <<"AD">>},
        {name, <<"Català"/utf8>>},
        {name_en, <<"Catalan"/utf8>>}
    ]},
    {<<"ce">>, [
        {language, <<"ce">>},
        {script, <<"Cyrl">>},
        {region, <<"RU">>},
        {name, <<"нохчийн"/utf8>>},
        {name_en, <<"Chechen"/utf8>>}
    ]},
    {<<"ch">>, [
        {language, <<"ch">>},
        {region, <<"GU">>},
        {name, <<"Finu' Chamoru"/utf8>>},
        {name_en, <<"Chamorro"/utf8>>}
    ]},
    {<<"co">>, [
        {language, <<"co">>},
        {name, <<"Corsu"/utf8>>},
        {name_en, <<"Corsican"/utf8>>}
    ]},
    {<<"cs">>, [
        {language, <<"cs">>},
        {region, <<"CZ">>},
        {name, <<"Čeština"/utf8>>},
        {name_en, <<"Czech"/utf8>>}
    ]},
    %% Omitting Church Slavic
    {<<"cv">>, [
        {language, <<"cv">>},
        {region, <<"RU">>},
        {script, <<"Cyrl">>},
        {name, <<"Чӑвашла"/utf8>>},
        {name_en, <<"Chuvash"/utf8>>}
    ]},
    {<<"cy">>, [
        {language, <<"cy">>},
        {region, <<"GB">>},
        {name, <<"Cymraeg"/utf8>>},
        {name_en, <<"Welsh"/utf8>>}
    ]},
    {<<"de">>, [
        {language, <<"de">>},
        {name, <<"Deutsch"/utf8>>},
        {name_en, <<"German"/utf8>>},
        {sublanguages, [
            {<<"de-at">>, [
                {language, <<"de">>},
                {region, <<"AT">>},
                {name, <<"Deutsch - Österreich"/utf8>>},
                {name_en, <<"German - Austria"/utf8>>}
            ]},
            {<<"de-ch">>, [
                {language, <<"de">>},
                {region, <<"CH">>},
                {name, <<"Deutsch - Schweiz"/utf8>>},
                {name_en, <<"German - Switzerland"/utf8>>}
            ]},
            {<<"de-de">>, [
                {language, <<"de">>},
                {region, <<"DE">>},
                {name, <<"Deutsch - Deutschland"/utf8>>},
                {name_en, <<"German - Germany"/utf8>>}
            ]},
            {<<"de-li">>, [
                {language, <<"de">>},
                {region, <<"LI">>},
                {name, <<"Deutsch - Liechtenstein"/utf8>>},
                {name_en, <<"German - Liechtenstein"/utf8>>}
            ]},
            {<<"de-lu">>, [
                {language, <<"de">>},
                {region, <<"LU">>},
                {name, <<"Deutsch - Luxemburg"/utf8>>},
                {name_en, <<"German - Luxembourg"/utf8>>}
            ]}
        ]}
    ]},
    {<<"da">>, [
        {language, <<"da">>},
        {name, <<"Dansk"/utf8>>},
        {name_en, <<"Danish"/utf8>>}
    ]},
    {<<"dz">>, [
        {language, <<"dz">>},
        {script, <<"Tibt">>},
        {region, <<"BT">>},
        {name, <<"རྫོང་ཁ་"/utf8>>},
        {name_en, <<"Dzongkha"/utf8>>}
    ]},
    {<<"el">>, [
        {language, <<"el">>},
        {script, <<"Grek">>},
        {name, <<"Ελληνικά"/utf8>>},
        {name_en, <<"Greek"/utf8>>}
    ]},
    {<<"en">>, [
        {language, <<"en">>},
        {name, <<"English"/utf8>>},
        {name_en, <<"English"/utf8>>},
        {sublanguages, [
            {<<"en-au">>, [
                {language, <<"en">>},
                {region, <<"AU">>},
                {name, <<"English - Australia"/utf8>>},
                {name_en, <<"English - Australia"/utf8>>}
            ]},
            {<<"en-bz">>, [
                {language, <<"en">>},
                {region, <<"BZ">>},
                {name, <<"English - Belize"/utf8>>},
                {name_en, <<"English - Belize"/utf8>>}
            ]},
            {<<"en-ca">>, [
                {language, <<"en">>},
                {region, <<"CA">>},
                {name, <<"English - Canada"/utf8>>},
                {name_en, <<"English - Canada"/utf8>>}
            ]},
            {<<"en-cb">>, [
                {language, <<"en">>},
                {region, <<"CB">>},
                {name, <<"English - Caribbean"/utf8>>},
                {name_en, <<"English - Caribbean"/utf8>>}
            ]},
            {<<"en-gb">>, [
                {language, <<"en">>},
                {region, <<"GB">>},
                {name, <<"English - United Kingdom"/utf8>>},
                {name_en, <<"English - United Kingdom"/utf8>>}
            ]},
            {<<"en-ie">>, [
                {language, <<"en">>},
                {region, <<"IE">>},
                {name, <<"English - Ireland"/utf8>>},
                {name_en, <<"English - Ireland"/utf8>>}
            ]},
            {<<"en-jm">>, [
                {language, <<"en">>},
                {region, <<"JM">>},
                {name, <<"English - Jamaica"/utf8>>},
                {name_en, <<"English - Jamaica"/utf8>>}
            ]},
            {<<"en-nz">>, [
                {language, <<"en">>},
                {region, <<"NZ">>},
                {name, <<"English - New Zealand"/utf8>>},
                {name_en, <<"English - New Zealand"/utf8>>}
            ]},
            {<<"en-ph">>, [
                {language, <<"en">>},
                {region, <<"PH">>},
                {name, <<"English - Republic of the Philippines"/utf8>>},
                {name_en, <<"English - Republic of the Philippines"/utf8>>}
            ]},
            {<<"en-tt">>, [
                {language, <<"en">>},
                {region, <<"TT">>},
                {name, <<"English - Trinidad and Tobago"/utf8>>},
                {name_en, <<"English - Trinidad and Tobago"/utf8>>}
            ]},
            {<<"en-us">>, [
                {language, <<"en">>},
                {region, <<"US">>},
                {name, <<"English - United States"/utf8>>},
                {name_en, <<"English - United States"/utf8>>}
            ]},
            {<<"en-za">>, [
                {language, <<"en">>},
                {region, <<"ZA">>},
                {name, <<"English - South Africa"/utf8>>},
                {name_en, <<"English - South Africa"/utf8>>}
            ]},
            {<<"en-zw">>, [
                {language, <<"en">>},
                {region, <<"ZW">>},
                {name, <<"English - Zimbabwe"/utf8>>},
                {name_en, <<"English - Zimbabwe"/utf8>>}
            ]}
        ]}
    ]},
    {<<"eo">>, [
        {language, <<"eo">>},
        {name, <<"Esperanto"/utf8>>},
        {name_en, <<"Esperanto"/utf8>>}
    ]},
    {<<"es">>, [
        {language, <<"es">>},
        {name, <<"Español"/utf8>>},
        {name_en, <<"Spanish"/utf8>>},
        {sublanguages, [
            {<<"es-419">>, [
                {language, <<"es">>},
                {region, <<"419">>},
                {name, <<"Español latinoamericano"/utf8>>},
                {name_en, <<"Spanish - Latin America and the Caribbean"/utf8>>}
            ]},
            {<<"es-ar">>, [
                {language, <<"es">>},
                {region, <<"AR">>},
                {name, <<"Español - Argentina"/utf8>>},
                {name_en, <<"Spanish - Argentina"/utf8>>}
            ]},
            {<<"es-bo">>, [
                {language, <<"es">>},
                {region, <<"BO">>},
                {name, <<"Español - Bolivia"/utf8>>},
                {name_en, <<"Spanish - Bolivia"/utf8>>}
            ]},
            {<<"es-cl">>, [
                {language, <<"es">>},
                {region, <<"CL">>},
                {name, <<"Español - Chile"/utf8>>},
                {name_en, <<"Spanish - Chile"/utf8>>}
            ]},
            {<<"es-co">>, [
                {language, <<"es">>},
                {region, <<"CO">>},
                {name, <<"Español - Colombia"/utf8>>},
                {name_en, <<"Spanish - Colombia"/utf8>>}
            ]},
            {<<"es-cr">>, [
                {language, <<"es">>},
                {region, <<"CR">>},
                {name, <<"Español - Costa Rica"/utf8>>},
                {name_en, <<"Spanish - Costa Rica"/utf8>>}
            ]},
            {<<"es-do">>, [
                {language, <<"es">>},
                {region, <<"DO">>},
                {name, <<"Español - República Dominicana"/utf8>>},
                {name_en, <<"Spanish - Dominican Republic"/utf8>>}
            ]},
            {<<"es-ec">>, [
                {language, <<"es">>},
                {region, <<"EC">>},
                {name, <<"Español - Ecuador"/utf8>>},
                {name_en, <<"Spanish - Ecuador"/utf8>>}
            ]},
            {<<"es-es">>, [
                {language, <<"es">>},
                {region, <<"ES">>},
                {name, <<"Español - España"/utf8>>},
                {name_en, <<"Spanish - Spain"/utf8>>}
            ]},
            {<<"es-gt">>, [
                {language, <<"es">>},
                {region, <<"GT">>},
                {name, <<"Español - Guatemala"/utf8>>},
                {name_en, <<"Spanish - Guatemala"/utf8>>}
            ]},
            {<<"es-hn">>, [
                {language, <<"es">>},
                {region, <<"HN">>},
                {name, <<"Español - Honduras"/utf8>>},
                {name_en, <<"Spanish - Honduras"/utf8>>}
            ]},
            {<<"es-mx">>, [
                {language, <<"es">>},
                {region, <<"MX">>},
                {name, <<"Español - México"/utf8>>},
                {name_en, <<"Spanish - Mexico"/utf8>>}
            ]},
            {<<"es-ni">>, [
                {language, <<"es">>},
                {region, <<"NI">>},
                {name, <<"Español - Nicaragua"/utf8>>},
                {name_en, <<"Spanish - Nicaragua"/utf8>>}
            ]},
            {<<"es-pa">>, [
                {language, <<"es">>},
                {region, <<"PA">>},
                {name, <<"Español - Panamá"/utf8>>},
                {name_en, <<"Spanish - Panama"/utf8>>}
            ]},
            {<<"es-pe">>, [
                {language, <<"es">>},
                {region, <<"PE">>},
                {name, <<"Español - Perú"/utf8>>},
                {name_en, <<"Spanish - Peru"/utf8>>}
            ]},
            {<<"es-pr">>, [
                {language, <<"es">>},
                {region, <<"PR">>},
                {name, <<"Español - Puerto Rico"/utf8>>},
                {name_en, <<"Spanish - Puerto Rico"/utf8>>}
            ]},
            {<<"es-py">>, [
                {language, <<"es">>},
                {region, <<"PY">>},
                {name, <<"Español - Paraguay"/utf8>>},
                {name_en, <<"Spanish - Paraguay"/utf8>>}
            ]},
            {<<"es-sv">>, [
                {language, <<"es">>},
                {region, <<"SV">>},
                {name, <<"Español - El Salvador"/utf8>>},
                {name_en, <<"Spanish - El Salvador"/utf8>>}
            ]},
            {<<"es-uy">>, [
                {language, <<"es">>},
                {region, <<"UY">>},
                {name, <<"Español - Uruguay"/utf8>>},
                {name_en, <<"Spanish - Uruguay"/utf8>>}
            ]},
            {<<"es-ve">>, [
                {language, <<"es">>},
                {region, <<"VE">>},
                {name, <<"Español - Venezuela"/utf8>>},
                {name_en, <<"Spanish - Venezuela"/utf8>>}
            ]}
        ]}
    ]},
    {<<"et">>, [
        {language, <<"et">>},
        {region, <<"EE">>},
        {name, <<"Eesti"/utf8>>},
        {name_en, <<"Estonian"/utf8>>}
    ]},
    {<<"eu">>, [
        {language, <<"eu">>},
        {region, <<"ES">>},
        {name, <<"Euskara"/utf8>>},
        {name_en, <<"Basque"/utf8>>}
    ]},
    {<<"fa">>, [
        {language, <<"fa">>},
        {direction, <<"RTL">>},
        {script, <<"Arab">>},
        {name, <<"فارسی"/utf8>>},
        {name_en, <<"Persian"/utf8>>}
    ]},
    {<<"fi">>, [
        {language, <<"fi">>},
        {name, <<"Suomi"/utf8>>},
        {name_en, <<"Finnish"/utf8>>}
    ]},
    {<<"fj">>, [
        {language, <<"fj">>},
        {region, <<"FJ">>},
        {name, <<"Na Vosa Vakaviti"/utf8>>},
        {name_en, <<"Fijian"/utf8>>}
    ]},
    {<<"fr">>, [
        {language, <<"fr">>},
        {name, <<"Français"/utf8>>},
        {name_en, <<"French"/utf8>>},
        {sublanguages, [
            {<<"fr-be">>, [
                {language, <<"fr">>},
                {region, <<"BE">>},
                {name, <<"Français - Belgique"/utf8>>},
                {name_en, <<"French - Belgium"/utf8>>}
            ]},
            {<<"fr-ca">>, [
                {language, <<"fr">>},
                {region, <<"CA">>},
                {name, <<"Français - Canada"/utf8>>},
                {name_en, <<"French - Canada"/utf8>>}
            ]},
            {<<"fr-ch">>, [
                {language, <<"fr">>},
                {region, <<"CH">>},
                {name, <<"Français - Suisse"/utf8>>},
                {name_en, <<"French - Switzerland"/utf8>>}
            ]},
            {<<"fr-fr">>, [
                {language, <<"fr">>},
                {region, <<"FR">>},
                {name, <<"Français - France"/utf8>>},
                {name_en, <<"French - France"/utf8>>}
            ]},
            {<<"fr-lu">>, [
                {language, <<"fr">>},
                {region, <<"LU">>},
                {name, <<"Français - Luxembourg"/utf8>>},
                {name_en, <<"French - Luxembourg"/utf8>>}
            ]},
            {<<"fr-mc">>, [
                {language, <<"fr">>},
                {region, <<"MC">>},
                {name, <<"Français - Monaco"/utf8>>},
                {name_en, <<"French - Monaco"/utf8>>}
            ]}
        ]}
    ]},
    {<<"fo">>, [
        {language, <<"fo">>},
        {region, <<"FO">>},
        {name, <<"Føroyskt"/utf8>>},
        {name_en, <<"Faroese"/utf8>>}
    ]},
    {<<"fy">>, [
        {language, <<"fy">>},
        {region, <<"NL">>},
        {name, <<"West-Frysk"/utf8>>},
        {name_en, <<"Frisian"/utf8>>}
    ]},
    {<<"ga">>, [
        {language, <<"ga">>},
        {name, <<"Gaeilge"/utf8>>},
        {name_en, <<"Gaelic"/utf8>>}
    ]},
    {<<"gd">>, [
        {language, <<"gd">>},
        {region, <<"GB">>},
        {name, <<"Gàidhlig"/utf8>>},
        {name_en, <<"Scottish Gaelic"/utf8>>}
    ]},
    {<<"gl">>, [
        {language, <<"gl">>},
        {region, <<"ES">>},
        {name, <<"Galego"/utf8>>},
        {name_en, <<"Galician"/utf8>>}
    ]},
    {<<"gn">>, [
        {language, <<"gn">>},
        {region, <<"PY">>},
        {name, <<"Avañe'ẽ"/utf8>>},
        {name_en, <<"Guarani"/utf8>>}
    ]},
    {<<"gu">>, [
        {language, <<"gu">>},
        {script, <<"Gujr">>},
        {name, <<"ગુજરાતી"/utf8>>},
        {name_en, <<"Gujarati"/utf8>>}
    ]},
    {<<"he">>, [
        {language, <<"he">>},
        {direction, <<"RTL">>},
        {script, <<"Hebr">>},
        {region, <<"IL">>},
        {name, <<"עברית"/utf8>>},
        {name_en, <<"Hebrew"/utf8>>}
    ]},
    {<<"hi">>, [
        {language, <<"hi">>},
        {script, <<"Deva">>},
        {name, <<"हिन्दी"/utf8>>},
        {name_en, <<"Hindi"/utf8>>}
    ]},
    {<<"hr">>, [
        {language, <<"hr">>},
        {name, <<"Hrvatski"/utf8>>},
        {name_en, <<"Croatian"/utf8>>},
        {sublanguages, [
            {<<"hr-ba">>, [
                {language, <<"hr">>},
                {region, <<"BA">>},
                {name, <<"hrvatski - Bosna i Hercegovina"/utf8>>},
                {name_en, <<"Croatian - Bosnia and Herzegovina"/utf8>>}
            ]},
            {<<"hr-hr">>, [
                {language, <<"hr">>},
                {region, <<"HR">>},
                {name, <<"hrvatski - Hrvatska"/utf8>>},
                {name_en, <<"Croatian - Croatia"/utf8>>}
            ]}
        ]}
    ]},
    {<<"hu">>, [
        {language, <<"hu">>},
        {name, <<"Magyar"/utf8>>},
        {name_en, <<"Hungarian"/utf8>>}
    ]},
    {<<"id">>, [
        {language, <<"id">>},
        {region, <<"ID">>},
        {name, <<"Indonesia"/utf8>>},
        {name_en, <<"Indonesian"/utf8>>}
    ]},
    {<<"ia">>, [
        {language, <<"ia">>},
        {name, <<"Interlingua"/utf8>>},
        {name_en, <<"Interlingua"/utf8>>}
    ]},
    {<<"is">>, [
        {language, <<"is">>},
        {region, <<"IS">>},
        {name, <<"Íslenska"/utf8>>},
        {name_en, <<"Islandic"/utf8>>}
    ]},
    {<<"it">>, [
        {language, <<"it">>},
        {name, <<"Italiano"/utf8>>},
        {name_en, <<"Italian"/utf8>>},
        {sublanguages, [
            {<<"it-ch">>, [
                {language, <<"it">>},
                {region, <<"CH">>},
                {name, <<"italiano - Svizzera"/utf8>>},
                {name_en, <<"Italian - Switzerland"/utf8>>}
            ]},
            {<<"it-it">>, [
                {language, <<"it">>},
                {region, <<"IT">>},
                {name, <<"italiano - Italia"/utf8>>},
                {name_en, <<"Italian - Italy"/utf8>>}
            ]}
        ]}
    ]},
    {<<"ja">>, [
        {language, <<"ja">>},
        {script, <<"Jpan">>}, % alias for Han + Hiragana + Katakana
        {name, <<"日本語"/utf8>>},
        {name_en, <<"Japanese"/utf8>>}
    ]},
    {<<"jv">>, [
        {language, <<"jv">>},
        {region, <<"ID">>},
        {name, <<"Basa jawa"/utf8>>},
        {name_en, <<"Javanese"/utf8>>}
    ]},
    {<<"ka">>, [
        {language, <<"ka">>},
        {script, <<"Geor">>},
        {region, <<"GE">>},
        {name, <<"ქართული"/utf8>>},
        {name_en, <<"Georgian"/utf8>>}
    ]},
    {<<"ko">>, [
        {language, <<"ko">>},
        {script, <<"Kore">>},
        {name, <<"한국어"/utf8>>},
        {name_en, <<"Korean"/utf8>>}
    ]},
    {<<"ku">>, [
        {language, <<"ku">>},
        {name, <<"Kurdî‎"/utf8>>},
        {name_en, <<"Kurdish"/utf8>>}
    ]},
    {<<"lt">>, [
        {language, <<"lt">>},
        {name, <<"Lietuvių"/utf8>>},
        {name_en, <<"Lithuanian"/utf8>>}
    ]},
    {<<"lv">>, [
        {language, <<"lv">>},
        {region, <<"LV">>},
        {name, <<"Latviešu"/utf8>>},
        {name_en, <<"Latvian"/utf8>>}
    ]},
    {<<"mg">>, [
        {language, <<"mg">>},
        {region, <<"MG">>},
        {name, <<"Malagasy"/utf8>>},
        {name_en, <<"Malagasy"/utf8>>}
    ]},
    {<<"mk">>, [
        {language, <<"mk">>},
        {region, <<"MK">>},
        {script, <<"Cyrl">>},
        {name, <<"македонски"/utf8>>},
        {name_en, <<"Macedonian"/utf8>>}
    ]},
    {<<"mn">>, [
        {language, <<"mn">>},
        {name, <<"монгол"/utf8>>},
        {name_en, <<"Mongolian"/utf8>>}
    ]},
    {<<"mt">>, [
        {language, <<"mt">>},
        {region, <<"MT">>},
        {name, <<"Malti"/utf8>>},
        {name_en, <<"Maltese"/utf8>>}
    ]},
    {<<"ms">>, [
        {language, <<"ms">>},
        {name, <<"Malay"/utf8>>},
        {name_en, <<"Malay"/utf8>>}
    ]},
    {<<"nl">>, [
        {language, <<"nl">>},
        {name, <<"Nederlands"/utf8>>},
        {name_en, <<"Dutch"/utf8>>},
        {sublanguages, [
            {<<"nl-be">>, [
                {language, <<"nl">>},
                {region, <<"BE">>},
                {name, <<"Vlaams - België"/utf8>>},
                {name_en, <<"Flemish - Belgium"/utf8>>}
            ]},
            {<<"nl-nl">>, [
                {language, <<"nl">>},
                {region, <<"NL">>},
                {name, <<"Nederlands - Nederland"/utf8>>},
                {name_en, <<"Dutch - Netherlands"/utf8>>}
            ]}
        ]}
    ]},
    {<<"no">>, [
        {language, <<"no">>},
        {name, <<"Norsk"/utf8>>},
        {name_en, <<"Norwegian"/utf8>>}
    ]},
    {<<"nn">>, [
        {language, <<"nn">>},
        {region, <<"NO">>},
        {name, <<"nynorsk"/utf8>>},
        {name_en, <<"Norwegian Nynorsk"/utf8>>}
    ]},
    {<<"pa">>, [
        {language, <<"pa">>},
        {script, <<"Arab">>},
        {name, <<"ਪੰਜਾਬੀ"/utf8>>},
        {name_en, <<"Punjabi"/utf8>>},
        {sublanguages, [
            {<<"pa-arab">>, [
                {language, <<"pa">>},
                {script, <<"Arab">>},
                {name, <<"ابی"/utf8>>},
                {name_en, <<"Punjabi - Arab"/utf8>>}
            ]},
            {<<"pa-guru">>, [
                {language, <<"pa">>},
                {script, <<"Guru">>},
                {name, <<"ਪੰਜਾਬੀ ਦੇ - ਗੁਰਮੁਖੀ"/utf8>>},
                {name_en, <<"Punjabi - Arab"/utf8>>}
            ]}
        ]}
    ]},
    {<<"pl">>, [
        {language, <<"pl">>},
        {region, <<"PL">>},
        {name, <<"Polski"/utf8>>},
        {name_en, <<"Polish"/utf8>>}
    ]},
    {<<"ps">>, [
        {language, <<"ps">>},
        {script, <<"Arab">>},
        {name, <<"تو"/utf8>>},
        {name_en, <<"Pashto"/utf8>>}
    ]},
    {<<"pt">>, [
        {language, <<"pt">>},
        {name, <<"Português"/utf8>>},
        {name_en, <<"Portuguese"/utf8>>},
        {sublanguages, [
            {<<"pt-br">>, [
                {language, <<"pt">>},
                {region, <<"BR">>},
                {name, <<"Português - Brasil"/utf8>>},
                {name_en, <<"Portuguese - Brazil"/utf8>>}
            ]},
            {<<"pt-pt">>, [
                {language, <<"pt">>},
                {region, <<"PT">>},
                {name, <<"Português - Portugal"/utf8>>},
                {name_en, <<"Portuguese - Portugal"/utf8>>}
            ]}
        ]}
    ]},
    {<<"ro">>, [
        {language, <<"ro">>},
        {name, <<"Română"/utf8>>},
        {name_en, <<"Romanian"/utf8>>}
    ]},
    {<<"ru">>, [
        {language, <<"ru">>},
        {script, <<"Cyrl">>},
        {name, <<"русский язык"/utf8>>},
        {name_en, <<"Russian"/utf8>>}
    ]},
    {<<"si">>, [
        {language, <<"si">>},
        {name, <<"Sinhalese"/utf8>>},
        {name_en, <<"Sinhalese"/utf8>>}
    ]},
    {<<"sk">>, [
        {language, <<"sk">>},
        {name, <<"Slovenčina"/utf8>>},
        {name_en, <<"Slovak"/utf8>>}
    ]},
    {<<"sl">>, [
        {language, <<"sl">>},
        {name, <<"Slovenščina"/utf8>>},
        {name_en, <<"Slovenian"/utf8>>}
    ]},
    {<<"sr">>, [
        {language, <<"sr">>},
        {script, <<"Cyrl">>},
        {name, <<"српски"/utf8>>},
        {name_en, <<"Serbian"/utf8>>}
    ]},
    {<<"su">>, [
        {language, <<"su">>},
        {name, <<"Sundanese"/utf8>>},
        {name_en, <<"Sundanese"/utf8>>}
    ]},
    {<<"sv">>, [
        {language, <<"sv">>},
        {name, <<"Svenska"/utf8>>},
        {name_en, <<"Swedish"/utf8>>}
    ]},
    {<<"sw">>, [
        {language, <<"sw">>},
        {name, <<"Kiswahili"/utf8>>},
        {name_en, <<"Swahili"/utf8>>}
    ]},
    {<<"sq">>, [
        {language, <<"sq">>},
        {name, <<"Shqip"/utf8>>},
        {name_en, <<"Albanian"/utf8>>}
    ]},
    {<<"ta">>, [
        {language, <<"ta">>},
        {name, <<"Tamil"/utf8>>},
        {name_en, <<"Tamil"/utf8>>}
    ]},
    {<<"th">>, [
        {language, <<"th">>},
        {script, <<"Thai">>},
        {name, <<"ไทย"/utf8>>},
        {name_en, <<"Thai"/utf8>>}
    ]},
    {<<"tl">>, [
        {language, <<"tl">>},
        {name, <<"Tagalog"/utf8>>},
        {name_en, <<"Tagalog"/utf8>>}
    ]},
    {<<"tr">>, [
        {language, <<"tr">>},
        {name, <<"Türkçe"/utf8>>},
        {name_en, <<"Turkish"/utf8>>}
    ]},
    {<<"uk">>, [
        {language, <<"uk">>},
        {script, <<"Cyrl">>},
        {name, <<"українська"/utf8>>},
        {name_en, <<"Ukrainian"/utf8>>}
    ]},
    {<<"ur">>, [
        {language, <<"ur">>},
        % {script, <<"Nastaʼlīq"/utf8>>},
        {name, <<"اُردُو"/utf8>>},
        {name_en, <<"Urdu"/utf8>>}
    ]},
    {<<"vi">>, [
        {language, <<"vi">>},
        {region, <<"VN">>},
        {name, <<"Tiếng Việt"/utf8>>},
        {name_en, <<"Vietnamese"/utf8>>}
    ]},
    {<<"xh">>, [
        {language, <<"xh">>},
        {name, <<"Xhosa"/utf8>>},
        {name_en, <<"Xhosa"/utf8>>}
    ]},
    {<<"yo">>, [
        {language, <<"yo">>},
        {name, <<"Èdè Yorùbá"/utf8>>},
        {name_en, <<"Yoruba"/utf8>>}
    ]},
    {<<"zh">>, [
        {language, <<"zh">>},
        {alias, [
            <<"zh-hans">>,
            <<"zh-hans-cn">>, <<"zh-cn">>,
            <<"zh-hans-sg">>, <<"zh-sg">>
        ]},
        {script, <<"Hans">>},
        {name, <<"中文"/utf8>>},
        {name_en, <<"Chinese (Simplified)"/utf8>>},
        {fallback, <<"zh-hant">>}
    ]},
    {<<"zh-hant">>, [
        {language, <<"zh-hant">>},
        {alias, [
            <<"zh-hk">>, <<"zh-hant-hk">>,
            <<"zh-tw">>, <<"zh-hant-tw">>,
            <<"zh-mo">>, <<"zh-hant-mo">>
        ]},
        {script, <<"Hant">>},
        {name, <<"中國傳統的腳本"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>},
        {fallback, <<"zh">>}
    ]},
    {<<"zlm">>, [{language, <<"zlm">>}, {name, <<"Malay"/utf8>>}, {name_en, <<"Malay"/utf8>>}]},
    {<<"urd">>, [{language, <<"urd">>}, {name, <<"Urdu"/utf8>>}, {name_en, <<"Urdu"/utf8>>}]},
    {<<"lah">>, [{language, <<"lah">>}, {name, <<"Lahnda"/utf8>>}, {name_en, <<"Lahnda"/utf8>>}]},
    {<<"pnb">>, [{language, <<"pnb">>}, {name, <<"Western Panjabi"/utf8>>}, {name_en, <<"Western Panjabi"/utf8>>}]},
    {<<"jav">>, [{language, <<"jav">>}, {name, <<"Javanese"/utf8>>}, {name_en, <<"Javanese"/utf8>>}]},
    {<<"pan">>, [{language, <<"pan">>}, {name, <<"Panjabi"/utf8>>}, {name_en, <<"Panjabi"/utf8>>}]},
    {<<"swh">>, [{language, <<"swh">>}, {name, <<"Swahili"/utf8>>}, {name_en, <<"Swahili"/utf8>>}]},
    {<<"wuu">>, [{language, <<"wuu">>}, {name, <<"Wu Chinese"/utf8>>}, {name_en, <<"Wu Chinese"/utf8>>}]},
    {<<"mar">>, [{language, <<"mar">>}, {name, <<"Marathi"/utf8>>}, {name_en, <<"Marathi"/utf8>>}]},
    {<<"pus">>, [{language, <<"pus">>}, {name, <<"Pashto"/utf8>>}, {name_en, <<"Pashto"/utf8>>}]},
    {<<"yue">>, [{language, <<"yue">>}, {name, <<"Yue Chinese"/utf8>>}, {name_en, <<"Yue Chinese"/utf8>>}]},
    {<<"pes">>, [{language, <<"pes">>}, {name, <<"Iranian Persian"/utf8>>}, {name_en, <<"Iranian Persian"/utf8>>}]},
    {<<"ori">>, [{language, <<"ori">>}, {name, <<"Oriya"/utf8>>}, {name_en, <<"Oriya"/utf8>>}]},
    {<<"ory">>, [{language, <<"ory">>}, {name, <<"Odia"/utf8>>}, {name_en, <<"Odia"/utf8>>}]},
    {<<"orm">>, [{language, <<"orm">>}, {name, <<"Oromo"/utf8>>}, {name_en, <<"Oromo"/utf8>>}]},
    {<<"gaz">>, [{language, <<"gaz">>}, {name, <<"West Central Oromo"/utf8>>}, {name_en, <<"West Central Oromo"/utf8>>}]},
    {<<"hau">>, [{language, <<"hau">>}, {name, <<"Hausa"/utf8>>}, {name_en, <<"Hausa"/utf8>>}]},
    {<<"sun">>, [{language, <<"sun">>}, {name, <<"Sundanese"/utf8>>}, {name_en, <<"Sundanese"/utf8>>}]},
    {<<"mal">>, [{language, <<"mal">>}, {name, <<"Malayalam"/utf8>>}, {name_en, <<"Malayalam"/utf8>>}]},
    {<<"hsn">>, [{language, <<"hsn">>}, {name, <<"Xiang"/utf8>>}, {name_en, <<"Xiang"/utf8>>}]},
    {<<"hak">>, [{language, <<"hak">>}, {name, <<"Hakka"/utf8>>}, {name_en, <<"Hakka"/utf8>>}]},
    {<<"kur">>, [{language, <<"kur">>}, {name, <<"Kurdish"/utf8>>}, {name_en, <<"Kurdish"/utf8>>}]},
    {<<"mai">>, [{language, <<"mai">>}, {name, <<"Maithili"/utf8>>}, {name_en, <<"Maithili"/utf8>>}]},
    {<<"pcm">>, [{language, <<"pcm">>}, {name, <<"Nigerian Pidgin"/utf8>>}, {name_en, <<"Nigerian Pidgin"/utf8>>}]},
    {<<"yor">>, [{language, <<"yor">>}, {name, <<"Yoruba"/utf8>>}, {name_en, <<"Yoruba"/utf8>>}]},
    {<<"uzb">>, [{language, <<"uzb">>}, {name, <<"Uzbek"/utf8>>}, {name_en, <<"Uzbek"/utf8>>}]},
    {<<"ibo">>, [{language, <<"ibo">>}, {name, <<"Igbo "/utf8>>}, {name_en, <<"Igbo "/utf8>>}]},
    {<<"lao">>, [{language, <<"lao">>}, {name, <<"Lao-Isan"/utf8>>}, {name_en, <<"Lao-Isan"/utf8>>}]},
    {<<"ful">>, [{language, <<"ful">>}, {name, <<"Fula"/utf8>>}, {name_en, <<"Fula"/utf8>>}]},
    {<<"snd">>, [{language, <<"snd">>}, {name, <<"Sindhi"/utf8>>}, {name_en, <<"Sindhi"/utf8>>}]},
    {<<"gan">>, [{language, <<"gan">>}, {name, <<"Gan"/utf8>>}, {name_en, <<"Gan"/utf8>>}]},
    {<<"mwr">>, [{language, <<"mwr">>}, {name, <<"Marwari"/utf8>>}, {name_en, <<"Marwari"/utf8>>}]},
    {<<"rwr">>, [{language, <<"rwr">>}, {name, <<"Marwari (India)"/utf8>>}, {name_en, <<"Marwari (India)"/utf8>>}]},
    {<<"wry">>, [{language, <<"wry">>}, {name, <<"Merwari"/utf8>>}, {name_en, <<"Merwari"/utf8>>}]},
    {<<"amh">>, [{language, <<"amh">>}, {name, <<"Amharic"/utf8>>}, {name_en, <<"Amharic"/utf8>>}]},
    {<<"uzn">>, [{language, <<"uzn">>}, {name, <<"Northern Uzbek"/utf8>>}, {name_en, <<"Northern Uzbek"/utf8>>}]},
    {<<"pbu">>, [{language, <<"pbu">>}, {name, <<"Northern Pashto"/utf8>>}, {name_en, <<"Northern Pashto"/utf8>>}]},
    {<<"kmr">>, [{language, <<"kmr">>}, {name, <<"Northern Kurdish"/utf8>>}, {name_en, <<"Northern Kurdish"/utf8>>}]},
    {<<"skr">>, [{language, <<"skr">>}, {name, <<"Saraiki"/utf8>>}, {name_en, <<"Saraiki"/utf8>>}]},
    {<<"raj">>, [{language, <<"raj">>}, {name, <<"Rajasthani"/utf8>>}, {name_en, <<"Rajasthani"/utf8>>}]},
    {<<"hbs">>, [{language, <<"hbs">>}, {name, <<"Serbo-Croatian"/utf8>>}, {name_en, <<"Serbo-Croatian"/utf8>>}]},
    {<<"hne">>, [{language, <<"hne">>}, {name, <<"Chhattisgarhi"/utf8>>}, {name_en, <<"Chhattisgarhi"/utf8>>}]},
    {<<"sin">>, [{language, <<"sin">>}, {name, <<"Sinhalese"/utf8>>}, {name_en, <<"Sinhalese"/utf8>>}]},
    {<<"ctg">>, [{language, <<"ctg">>}, {name, <<"Chittagonian"/utf8>>}, {name_en, <<"Chittagonian"/utf8>>}]},
    {<<"khm">>, [{language, <<"khm">>}, {name, <<"Cambodian"/utf8>>}, {name_en, <<"Cambodian"/utf8>>}]},
    {<<"nep">>, [{language, <<"nep">>}, {name, <<"Nepali"/utf8>>}, {name_en, <<"Nepali"/utf8>>}]},
    {<<"zha">>, [{language, <<"zha">>}, {name, <<"Zhuang"/utf8>>}, {name_en, <<"Zhuang"/utf8>>}]},
    {<<"npi">>, [{language, <<"npi">>}, {name, <<"Nepali"/utf8>>}, {name_en, <<"Nepali"/utf8>>}]},
    {<<"ceb">>, [{language, <<"ceb">>}, {name, <<"Cebuano"/utf8>>}, {name_en, <<"Cebuano"/utf8>>}]},
    {<<"asm">>, [{language, <<"asm">>}, {name, <<"Assamese"/utf8>>}, {name_en, <<"Assamese"/utf8>>}]},
    {<<"mad">>, [{language, <<"mad">>}, {name, <<"Madurese"/utf8>>}, {name_en, <<"Madurese"/utf8>>}]},
    {<<"rkt">>, [{language, <<"rkt">>}, {name, <<"Rangpuri"/utf8>>}, {name_en, <<"Rangpuri"/utf8>>}]},
    {<<"tts">>, [{language, <<"tts">>}, {name, <<"Northeastern Thai"/utf8>>}, {name_en, <<"Northeastern Thai"/utf8>>}]},
    {<<"som">>, [{language, <<"som">>}, {name, <<"Somali"/utf8>>}, {name_en, <<"Somali"/utf8>>}]},
    {<<"mag">>, [{language, <<"mag">>}, {name, <<"Magahi"/utf8>>}, {name_en, <<"Magahi"/utf8>>}]},
    {<<"bar">>, [{language, <<"bar">>}, {name, <<"Bavarian"/utf8>>}, {name_en, <<"Bavarian"/utf8>>}]},
    {<<"tso">>, [{language, <<"tso">>}, {name, <<"Tsonga"/utf8>>}, {name_en, <<"Tsonga"/utf8>>}]},
    {<<"sna">>, [{language, <<"sna">>}, {name, <<"Shona"/utf8>>}, {name_en, <<"Shona"/utf8>>}]},
    {<<"run">>, [{language, <<"run">>}, {name, <<"Rundi"/utf8>>}, {name_en, <<"Rundi"/utf8>>}]},
    {<<"nya">>, [{language, <<"nya">>}, {name, <<"Chewa"/utf8>>}, {name_en, <<"Chewa"/utf8>>}]},
    {<<"zul">>, [{language, <<"zul">>}, {name, <<"Zulu"/utf8>>}, {name_en, <<"Zulu"/utf8>>}]},
    {<<"fuv">>, [{language, <<"fuv">>}, {name, <<"Nigerian Fulfulde"/utf8>>}, {name_en, <<"Nigerian Fulfulde"/utf8>>}]},
    {<<"aka">>, [{language, <<"aka">>}, {name, <<"Akan"/utf8>>}, {name_en, <<"Akan"/utf8>>}]},
    {<<"kaz">>, [{language, <<"kaz">>}, {name, <<"Kazakh"/utf8>>}, {name_en, <<"Kazakh"/utf8>>}]},
    {<<"mnp">>, [{language, <<"mnp">>}, {name, <<"Northern Min"/utf8>>}, {name_en, <<"Northern Min"/utf8>>}]},
    {<<"syl">>, [{language, <<"syl">>}, {name, <<"Sylheti"/utf8>>}, {name_en, <<"Sylheti"/utf8>>}]},
    {<<"dcc">>, [{language, <<"dcc">>}, {name, <<"Deccan"/utf8>>}, {name_en, <<"Deccan"/utf8>>}]},
    {<<"mnc">>, [{language, <<"mnc">>}, {name, <<"Manchu"/utf8>>}, {name_en, <<"Manchu"/utf8>>}]},
    {<<"uig">>, [{language, <<"uig">>}, {name, <<"Uyghur"/utf8>>}, {name_en, <<"Uyghur"/utf8>>}]},
    {<<"mup">>, [{language, <<"mup">>}, {name, <<"Malvi"/utf8>>}, {name_en, <<"Malvi"/utf8>>}]},
    {<<"kin">>, [{language, <<"kin">>}, {name, <<"Kinyarwanda"/utf8>>}, {name_en, <<"Kinyarwanda"/utf8>>}]},
    {<<"hat">>, [{language, <<"hat">>}, {name, <<"Haitian Creole"/utf8>>}, {name_en, <<"Haitian Creole"/utf8>>}]},
    {<<"dhd">>, [{language, <<"dhd">>}, {name, <<"Dhundari"/utf8>>}, {name_en, <<"Dhundari"/utf8>>}]},
    {<<"prs">>, [{language, <<"prs">>}, {name, <<"Dari"/utf8>>}, {name_en, <<"Dari"/utf8>>}]},
    {<<"swc">>, [{language, <<"swc">>}, {name, <<"Congo Swahili"/utf8>>}, {name_en, <<"Congo Swahili"/utf8>>}]},
    {<<"hil">>, [{language, <<"hil">>}, {name, <<"Hiligaynon"/utf8>>}, {name_en, <<"Hiligaynon"/utf8>>}]},
    {<<"ilo">>, [{language, <<"ilo">>}, {name, <<"Iloko"/utf8>>}, {name_en, <<"Iloko"/utf8>>}]},
    {<<"twi">>, [{language, <<"twi">>}, {name, <<"Twi"/utf8>>}, {name_en, <<"Twi"/utf8>>}]},
    {<<"que">>, [{language, <<"que">>}, {name, <<"Quechua "/utf8>>}, {name_en, <<"Quechua "/utf8>>}]},
    {<<"xho">>, [{language, <<"xho">>}, {name, <<"Xhosa"/utf8>>}, {name_en, <<"Xhosa"/utf8>>}]},
    {<<"grn">>, [{language, <<"grn">>}, {name, <<"Guaraní"/utf8>>}, {name_en, <<"Guaraní"/utf8>>}]},
    {<<"gug">>, [{language, <<"gug">>}, {name, <<"Paraguayan Guaraní"/utf8>>}, {name_en, <<"Paraguayan Guaraní"/utf8>>}]},
    {<<"hac">>, [{language, <<"hac">>}, {name, <<"Gurani"/utf8>>}, {name_en, <<"Gurani"/utf8>>}]},
    {<<"tir">>, [{language, <<"tir">>}, {name, <<"Tigrinya"/utf8>>}, {name_en, <<"Tigrinya"/utf8>>}]},
    {<<"tgk">>, [{language, <<"tgk">>}, {name, <<"Tajik"/utf8>>}, {name_en, <<"Tajik"/utf8>>}]},
    {<<"mos">>, [{language, <<"mos">>}, {name, <<"Mossi"/utf8>>}, {name_en, <<"Mossi"/utf8>>}]},
    {<<"plt">>, [{language, <<"plt">>}, {name, <<"Plateau Malagasy"/utf8>>}, {name_en, <<"Plateau Malagasy"/utf8>>}]},
    {<<"gom">>, [{language, <<"gom">>}, {name, <<"Goan Konkani"/utf8>>}, {name_en, <<"Goan Konkani"/utf8>>}]},
    {<<"knn">>, [{language, <<"knn">>}, {name, <<"Konkani"/utf8>>}, {name_en, <<"Konkani"/utf8>>}]},
    {<<"kok">>, [{language, <<"kok">>}, {name, <<"Konkani (macrolanguage)"/utf8>>}, {name_en, <<"Konkani (macrolanguage)"/utf8>>}]},
    {<<"vah">>, [{language, <<"vah">>}, {name, <<"Varhadi-Nagpuri"/utf8>>}, {name_en, <<"Varhadi-Nagpuri"/utf8>>}]},
    {<<"kik">>, [{language, <<"kik">>}, {name, <<"Kikuyu"/utf8>>}, {name_en, <<"Kikuyu"/utf8>>}]},
    {<<"vmw">>, [{language, <<"vmw">>}, {name, <<"Makhuwa"/utf8>>}, {name_en, <<"Makhuwa"/utf8>>}]},
    {<<"xsq">>, [{language, <<"xsq">>}, {name, <<"Makhuwa-Saka"/utf8>>}, {name_en, <<"Makhuwa-Saka"/utf8>>}]},
    {<<"kon">>, [{language, <<"kon">>}, {name, <<"Kongo"/utf8>>}, {name_en, <<"Kongo"/utf8>>}]},
    {<<"bak">>, [{language, <<"bak">>}, {name, <<"Tatar"/utf8>>}, {name_en, <<"Tatar"/utf8>>}]},
    {<<"tat">>, [{language, <<"tat">>}, {name, <<"Tatar"/utf8>>}, {name_en, <<"Tatar"/utf8>>}]},
    {<<"kng">>, [{language, <<"kng">>}, {name, <<"Koongo"/utf8>>}, {name_en, <<"Koongo"/utf8>>}]},
    {<<"pst">>, [{language, <<"pst">>}, {name, <<"Central Pashto"/utf8>>}, {name_en, <<"Central Pashto"/utf8>>}]},
    {<<"sat">>, [{language, <<"sat">>}, {name, <<"Santali"/utf8>>}, {name_en, <<"Santali"/utf8>>}]},
    {<<"lua">>, [{language, <<"lua">>}, {name, <<"Luba-Lulua"/utf8>>}, {name_en, <<"Luba-Lulua"/utf8>>}]},
    {<<"umb">>, [{language, <<"umb">>}, {name, <<"Umbundu"/utf8>>}, {name_en, <<"Umbundu"/utf8>>}]},
    {<<"lmn">>, [{language, <<"lmn">>}, {name, <<"Lambadi"/utf8>>}, {name_en, <<"Lambadi"/utf8>>}]},
    {<<"nod">>, [{language, <<"nod">>}, {name, <<"Northern Thai"/utf8>>}, {name_en, <<"Northern Thai"/utf8>>}]},
    {<<"nap">>, [{language, <<"nap">>}, {name, <<"Neapolitan"/utf8>>}, {name_en, <<"Neapolitan"/utf8>>}]},
    {<<"kas">>, [{language, <<"kas">>}, {name, <<"Kashmiri"/utf8>>}, {name_en, <<"Kashmiri"/utf8>>}]},
    {<<"sot">>, [{language, <<"sot">>}, {name, <<"Sotho"/utf8>>}, {name_en, <<"Sotho"/utf8>>}]},
    {<<"min">>, [{language, <<"min">>}, {name, <<"Minangkabau"/utf8>>}, {name_en, <<"Minangkabau"/utf8>>}]},
    {<<"kab">>, [{language, <<"kab">>}, {name, <<"Kabyle"/utf8>>}, {name_en, <<"Kabyle"/utf8>>}]},
    {<<"suk">>, [{language, <<"suk">>}, {name, <<"Sukuma"/utf8>>}, {name_en, <<"Sukuma"/utf8>>}]},
    {<<"ktu">>, [{language, <<"ktu">>}, {name, <<"Kituba (Democratic Republic of Congo)"/utf8>>}, {name_en, <<"Kituba (Democratic Republic of Congo)"/utf8>>}]},
    {<<"mon">>, [{language, <<"mon">>}, {name, <<"Mongolian"/utf8>>}, {name_en, <<"Mongolian"/utf8>>}]},
    {<<"khk">>, [{language, <<"khk">>}, {name, <<"Halh Mongolian"/utf8>>}, {name_en, <<"Halh Mongolian"/utf8>>}]},
    {<<"lug">>, [{language, <<"lug">>}, {name, <<"Ganda"/utf8>>}, {name_en, <<"Ganda"/utf8>>}]},
    {<<"mtr">>, [{language, <<"mtr">>}, {name, <<"Mewari"/utf8>>}, {name_en, <<"Mewari"/utf8>>}]},
    {<<"sag">>, [{language, <<"sag">>}, {name, <<"Sango"/utf8>>}, {name_en, <<"Sango"/utf8>>}]},
    {<<"gpe">>, [{language, <<"gpe">>}, {name, <<"Ghanaian Pidgin English"/utf8>>}, {name_en, <<"Ghanaian Pidgin English"/utf8>>}]},
    {<<"msc">>, [{language, <<"msc">>}, {name, <<"Sankaran Maninka"/utf8>>}, {name_en, <<"Sankaran Maninka"/utf8>>}]},
    {<<"mzj">>, [{language, <<"mzj">>}, {name, <<"Manya"/utf8>>}, {name_en, <<"Manya"/utf8>>}]},
    {<<"sdh">>, [{language, <<"sdh">>}, {name, <<"Southern Kurdish"/utf8>>}, {name_en, <<"Southern Kurdish"/utf8>>}]},
    {<<"wtm">>, [{language, <<"wtm">>}, {name, <<"Mewati"/utf8>>}, {name_en, <<"Mewati"/utf8>>}]},
    {<<"gsw">>, [{language, <<"gsw">>}, {name, <<"Swiss German"/utf8>>}, {name_en, <<"Swiss German"/utf8>>}]},
    {<<"vmf">>, [{language, <<"vmf">>}, {name, <<"Mainfränkisch"/utf8>>}, {name_en, <<"Mainfränkisch"/utf8>>}]},
    {<<"hoj">>, [{language, <<"hoj">>}, {name, <<"Hadothi"/utf8>>}, {name_en, <<"Hadothi"/utf8>>}]},
    {<<"scn">>, [{language, <<"scn">>}, {name, <<"Sicilian"/utf8>>}, {name_en, <<"Sicilian"/utf8>>}]},
    {<<"nso">>, [{language, <<"nso">>}, {name, <<"Pedi"/utf8>>}, {name_en, <<"Pedi"/utf8>>}]},
    {<<"czh">>, [{language, <<"czh">>}, {name, <<"Huizhou Chinese"/utf8>>}, {name_en, <<"Huizhou Chinese"/utf8>>}]},
    {<<"hae">>, [{language, <<"hae">>}, {name, <<"Eastern Oromo"/utf8>>}, {name_en, <<"Eastern Oromo"/utf8>>}]},
    {<<"sou">>, [{language, <<"sou">>}, {name, <<"Southern Thai"/utf8>>}, {name_en, <<"Southern Thai"/utf8>>}]},
    {<<"kir">>, [{language, <<"kir">>}, {name, <<"Kyrgyz"/utf8>>}, {name_en, <<"Kyrgyz"/utf8>>}]},
    {<<"kau">>, [{language, <<"kau">>}, {name, <<"Kanuri"/utf8>>}, {name_en, <<"Kanuri"/utf8>>}]},
    {<<"wol">>, [{language, <<"wol">>}, {name, <<"Wolof"/utf8>>}, {name_en, <<"Wolof"/utf8>>}]},
    {<<"aar">>, [{language, <<"aar">>}, {name, <<"Afar"/utf8>>}, {name_en, <<"Afar"/utf8>>}]},
    {<<"knc">>, [{language, <<"knc">>}, {name, <<"Central Kanuri"/utf8>>}, {name_en, <<"Central Kanuri"/utf8>>}]},
    {<<"luo">>, [{language, <<"luo">>}, {name, <<"Luo (Kenya and Tanzania)"/utf8>>}, {name_en, <<"Luo (Kenya and Tanzania)"/utf8>>}]},
    {<<"bam">>, [{language, <<"bam">>}, {name, <<"Bambara"/utf8>>}, {name_en, <<"Bambara"/utf8>>}]},
    {<<"kmb">>, [{language, <<"kmb">>}, {name, <<"Kimbundu"/utf8>>}, {name_en, <<"Kimbundu"/utf8>>}]},
    {<<"ksw">>, [{language, <<"ksw">>}, {name, <<"S'gaw Karen"/utf8>>}, {name_en, <<"S'gaw Karen"/utf8>>}]},
    {<<"rmt">>, [{language, <<"rmt">>}, {name, <<"Domari"/utf8>>}, {name_en, <<"Domari"/utf8>>}]},
    {<<"rom">>, [{language, <<"rom">>}, {name, <<"Romany"/utf8>>}, {name_en, <<"Romany"/utf8>>}]},
    {<<"shi">>, [{language, <<"shi">>}, {name, <<"Tachelhit"/utf8>>}, {name_en, <<"Tachelhit"/utf8>>}]},
    {<<"gax">>, [{language, <<"gax">>}, {name, <<"Borana-Arsi-Guji Oromo"/utf8>>}, {name_en, <<"Borana-Arsi-Guji Oromo"/utf8>>}]},
    {<<"vec">>, [{language, <<"vec">>}, {name, <<"Venetian"/utf8>>}, {name_en, <<"Venetian"/utf8>>}]},
    {<<"kam">>, [{language, <<"kam">>}, {name, <<"Kamba (Kenya)"/utf8>>}, {name_en, <<"Kamba (Kenya)"/utf8>>}]},
    {<<"fuc">>, [{language, <<"fuc">>}, {name, <<"Pulaar "/utf8>>}, {name_en, <<"Pulaar "/utf8>>}]},
    {<<"hmn">>, [{language, <<"hmn">>}, {name, <<"Hmong"/utf8>>}, {name_en, <<"Hmong"/utf8>>}]},
    {<<"ewe">>, [{language, <<"ewe">>}, {name, <<"Ewe"/utf8>>}, {name_en, <<"Ewe"/utf8>>}]},
    {<<"lmo">>, [{language, <<"lmo">>}, {name, <<"Lombard"/utf8>>}, {name_en, <<"Lombard"/utf8>>}]},
    {<<"emk">>, [{language, <<"emk">>}, {name, <<"Eastern Maninkakan"/utf8>>}, {name_en, <<"Eastern Maninkakan"/utf8>>}]},
    {<<"phr">>, [{language, <<"phr">>}, {name, <<"Pahari-Potwari"/utf8>>}, {name_en, <<"Pahari-Potwari"/utf8>>}]},
    {<<"bos">>, [{language, <<"bos">>}, {name, <<"Bosnian"/utf8>>}, {name_en, <<"Bosnian"/utf8>>}]},
    {<<"sck">>, [{language, <<"sck">>}, {name, <<"Sadri"/utf8>>}, {name_en, <<"Sadri"/utf8>>}]},
    {<<"war">>, [{language, <<"war">>}, {name, <<"Waray (Philippines)"/utf8>>}, {name_en, <<"Waray (Philippines)"/utf8>>}]},
    {<<"mvf">>, [{language, <<"mvf">>}, {name, <<"Peripheral Mongolian"/utf8>>}, {name_en, <<"Peripheral Mongolian"/utf8>>}]},
    {<<"shn">>, [{language, <<"shn">>}, {name, <<"Shan"/utf8>>}, {name_en, <<"Shan"/utf8>>}]},
    {<<"glk">>, [{language, <<"glk">>}, {name, <<"Gilaki"/utf8>>}, {name_en, <<"Gilaki"/utf8>>}]},
    {<<"mzn">>, [{language, <<"mzn">>}, {name, <<"Mazanderani"/utf8>>}, {name_en, <<"Mazanderani"/utf8>>}]},
    {<<"mey">>, [{language, <<"mey">>}, {name, <<"Hassaniyya"/utf8>>}, {name_en, <<"Hassaniyya"/utf8>>}]},
    {<<"czo">>, [{language, <<"czo">>}, {name, <<"Min Zhong Chinese"/utf8>>}, {name_en, <<"Min Zhong Chinese"/utf8>>}]},
    {<<"mui">>, [{language, <<"mui">>}, {name, <<"Musi"/utf8>>}, {name_en, <<"Musi"/utf8>>}]},
    {<<"yao">>, [{language, <<"yao">>}, {name, <<"Yao"/utf8>>}, {name_en, <<"Yao"/utf8>>}]},
    {<<"gdx">>, [{language, <<"gdx">>}, {name, <<"Godwari"/utf8>>}, {name_en, <<"Godwari"/utf8>>}]},
    {<<"ven">>, [{language, <<"ven">>}, {name, <<"Venda"/utf8>>}, {name_en, <<"Venda"/utf8>>}]},
    {<<"fuf">>, [{language, <<"fuf">>}, {name, <<"Pular"/utf8>>}, {name_en, <<"Pular"/utf8>>}]},
    {<<"gay">>, [{language, <<"gay">>}, {name, <<"Gayo"/utf8>>}, {name_en, <<"Gayo"/utf8>>}]},
    {<<"hrx">>, [{language, <<"hrx">>}, {name, <<"Hunsrik"/utf8>>}, {name_en, <<"Hunsrik"/utf8>>}]},
    {<<"msi">>, [{language, <<"msi">>}, {name, <<"Sabah Malay"/utf8>>}, {name_en, <<"Sabah Malay"/utf8>>}]},
    {<<"sid">>, [{language, <<"sid">>}, {name, <<"Sidamo"/utf8>>}, {name_en, <<"Sidamo"/utf8>>}]},
    {<<"swv">>, [{language, <<"swv">>}, {name, <<"Shekhawati"/utf8>>}, {name_en, <<"Shekhawati"/utf8>>}]},
    {<<"rjs">>, [{language, <<"rjs">>}, {name, <<"Rajbanshi"/utf8>>}, {name_en, <<"Rajbanshi"/utf8>>}]},
    {<<"uzs">>, [{language, <<"uzs">>}, {name, <<"Southern Uzbek"/utf8>>}, {name_en, <<"Southern Uzbek"/utf8>>}]},
    {<<"rki">>, [{language, <<"rki">>}, {name, <<"Rakhine"/utf8>>}, {name_en, <<"Rakhine"/utf8>>}]},
    {<<"gbm">>, [{language, <<"gbm">>}, {name, <<"Garhwali"/utf8>>}, {name_en, <<"Garhwali"/utf8>>}]},
    {<<"aym">>, [{language, <<"aym">>}, {name, <<"Aymara"/utf8>>}, {name_en, <<"Aymara"/utf8>>}]},
    {<<"quh">>, [{language, <<"quh">>}, {name, <<"South Bolivian Quechua"/utf8>>}, {name_en, <<"South Bolivian Quechua"/utf8>>}]},
    {<<"gon">>, [{language, <<"gon">>}, {name, <<"Gondi"/utf8>>}, {name_en, <<"Gondi"/utf8>>}]},
    {<<"myx">>, [{language, <<"myx">>}, {name, <<"Masaaba"/utf8>>}, {name_en, <<"Masaaba"/utf8>>}]},
    {<<"pcc">>, [{language, <<"pcc">>}, {name, <<"Bouyei"/utf8>>}, {name_en, <<"Bouyei"/utf8>>}]},
    {<<"jam">>, [{language, <<"jam">>}, {name, <<"Jamaican Creole English"/utf8>>}, {name_en, <<"Jamaican Creole English"/utf8>>}]},
    {<<"cpx">>, [{language, <<"cpx">>}, {name, <<"Puxian Min"/utf8>>}, {name_en, <<"Puxian Min"/utf8>>}]},
    {<<"meo">>, [{language, <<"meo">>}, {name, <<"Kedah Malay"/utf8>>}, {name_en, <<"Kedah Malay"/utf8>>}]},
    {<<"tum">>, [{language, <<"tum">>}, {name, <<"Tumbuka"/utf8>>}, {name_en, <<"Tumbuka"/utf8>>}]},
    {<<"dyu">>, [{language, <<"dyu">>}, {name, <<"Dyula"/utf8>>}, {name_en, <<"Dyula"/utf8>>}]},
    {<<"mfa">>, [{language, <<"mfa">>}, {name, <<"Pattani Malay"/utf8>>}, {name_en, <<"Pattani Malay"/utf8>>}]},
    {<<"wbr">>, [{language, <<"wbr">>}, {name, <<"Wagdi"/utf8>>}, {name_en, <<"Wagdi"/utf8>>}]},
    {<<"dje">>, [{language, <<"dje">>}, {name, <<"Zarma"/utf8>>}, {name_en, <<"Zarma"/utf8>>}]},
    {<<"kfy">>, [{language, <<"kfy">>}, {name, <<"Kumaoni"/utf8>>}, {name_en, <<"Kumaoni"/utf8>>}]},
    {<<"ndc">>, [{language, <<"ndc">>}, {name, <<"Ndau"/utf8>>}, {name_en, <<"Ndau"/utf8>>}]},
    {<<"tzm">>, [{language, <<"tzm">>}, {name, <<"Central Atlas Tamazight"/utf8>>}, {name_en, <<"Central Atlas Tamazight"/utf8>>}]},
    {<<"ssw">>, [{language, <<"ssw">>}, {name, <<"Swati"/utf8>>}, {name_en, <<"Swati"/utf8>>}]},
    {<<"nyn">>, [{language, <<"nyn">>}, {name, <<"Nyankole"/utf8>>}, {name_en, <<"Nyankole"/utf8>>}]},
    {<<"quc">>, [{language, <<"quc">>}, {name, <<"K'iche'"/utf8>>}, {name_en, <<"K'iche'"/utf8>>}]},
    {<<"dgo">>, [{language, <<"dgo">>}, {name, <<"Dogri"/utf8>>}, {name_en, <<"Dogri"/utf8>>}]},
    {<<"doi">>, [{language, <<"doi">>}, {name, <<"Dogri (macrolanguage)"/utf8>>}, {name_en, <<"Dogri (macrolanguage)"/utf8>>}]},
    {<<"fon">>, [{language, <<"fon">>}, {name, <<"Fon"/utf8>>}, {name_en, <<"Fon"/utf8>>}]},
    {<<"guz">>, [{language, <<"guz">>}, {name, <<"Gusii"/utf8>>}, {name_en, <<"Gusii"/utf8>>}]},
    {<<"haz">>, [{language, <<"haz">>}, {name, <<"Hazaragi"/utf8>>}, {name_en, <<"Hazaragi"/utf8>>}]},
    {<<"mxl">>, [{language, <<"mxl">>}, {name, <<"Maxi Gbe"/utf8>>}, {name_en, <<"Maxi Gbe"/utf8>>}]},
    {<<"noe">>, [{language, <<"noe">>}, {name, <<"Nimadi"/utf8>>}, {name_en, <<"Nimadi"/utf8>>}]},
    {<<"tiv">>, [{language, <<"tiv">>}, {name, <<"Tiv"/utf8>>}, {name_en, <<"Tiv"/utf8>>}]},
    {<<"lin">>, [{language, <<"lin">>}, {name, <<"Lingala"/utf8>>}, {name_en, <<"Lingala"/utf8>>}]},
    {<<"mak">>, [{language, <<"mak">>}, {name, <<"Makasar"/utf8>>}, {name_en, <<"Makasar"/utf8>>}]},
    {<<"sas">>, [{language, <<"sas">>}, {name, <<"Sasak"/utf8>>}, {name_en, <<"Sasak"/utf8>>}]},
    {<<"snk">>, [{language, <<"snk">>}, {name, <<"Soninke"/utf8>>}, {name_en, <<"Soninke"/utf8>>}]},
    {<<"xog">>, [{language, <<"xog">>}, {name, <<"Soga"/utf8>>}, {name_en, <<"Soga"/utf8>>}]},
    {<<"nde">>, [{language, <<"nde">>}, {name, <<"North Ndebele"/utf8>>}, {name_en, <<"North Ndebele"/utf8>>}]},
    {<<"iii">>, [{language, <<"iii">>}, {name, <<"Sichuan Yi"/utf8>>}, {name_en, <<"Sichuan Yi"/utf8>>}]},
    {<<"gmv">>, [{language, <<"gmv">>}, {name, <<"Gamo"/utf8>>}, {name_en, <<"Gamo"/utf8>>}]},
    {<<"kru">>, [{language, <<"kru">>}, {name, <<"Kurukh"/utf8>>}, {name_en, <<"Kurukh"/utf8>>}]},
    {<<"mer">>, [{language, <<"mer">>}, {name, <<"Meru"/utf8>>}, {name_en, <<"Meru"/utf8>>}]},
    {<<"sxu">>, [{language, <<"sxu">>}, {name, <<"Upper Saxon"/utf8>>}, {name_en, <<"Upper Saxon"/utf8>>}]},
    {<<"wes">>, [{language, <<"wes">>}, {name, <<"Cameroon Pidgin"/utf8>>}, {name_en, <<"Cameroon Pidgin"/utf8>>}]},
    {<<"zyb">>, [{language, <<"zyb">>}, {name, <<"Yongbei Zhuang"/utf8>>}, {name_en, <<"Yongbei Zhuang"/utf8>>}]},
    {<<"gno">>, [{language, <<"gno">>}, {name, <<"Northern Gondi"/utf8>>}, {name_en, <<"Northern Gondi"/utf8>>}]},
    {<<"fat">>, [{language, <<"fat">>}, {name, <<"Fanti"/utf8>>}, {name_en, <<"Fanti"/utf8>>}]},
    {<<"fbl">>, [{language, <<"fbl">>}, {name, <<"West Albay Bikol"/utf8>>}, {name_en, <<"West Albay Bikol"/utf8>>}]},
    {<<"khn">>, [{language, <<"khn">>}, {name, <<"Khandesi"/utf8>>}, {name_en, <<"Khandesi"/utf8>>}]},
    {<<"mfp">>, [{language, <<"mfp">>}, {name, <<"Makassar Malay"/utf8>>}, {name_en, <<"Makassar Malay"/utf8>>}]},
    {<<"nyf">>, [{language, <<"nyf">>}, {name, <<"Giryama"/utf8>>}, {name_en, <<"Giryama"/utf8>>}]},
    {<<"pam">>, [{language, <<"pam">>}, {name, <<"Pampanga"/utf8>>}, {name_en, <<"Pampanga"/utf8>>}]},
    {<<"sgc">>, [{language, <<"sgc">>}, {name, <<"Kipsigis"/utf8>>}, {name_en, <<"Kipsigis"/utf8>>}]},
    {<<"teo">>, [{language, <<"teo">>}, {name, <<"Teso"/utf8>>}, {name_en, <<"Teso"/utf8>>}]},
    {<<"ymm">>, [{language, <<"ymm">>}, {name, <<"Maay"/utf8>>}, {name_en, <<"Maay"/utf8>>}]},
    {<<"hno">>, [{language, <<"hno">>}, {name, <<"Northern Hindko"/utf8>>}, {name_en, <<"Northern Hindko"/utf8>>}]},
    {<<"rhg">>, [{language, <<"rhg">>}, {name, <<"Rohingya"/utf8>>}, {name_en, <<"Rohingya"/utf8>>}]},
    {<<"zyn">>, [{language, <<"zyn">>}, {name, <<"Yongnan Zhuang"/utf8>>}, {name_en, <<"Yongnan Zhuang"/utf8>>}]},
    {<<"zzj">>, [{language, <<"zzj">>}, {name, <<"Zuojiang Zhuang"/utf8>>}, {name_en, <<"Zuojiang Zhuang"/utf8>>}]},
    {<<"rif">>, [{language, <<"rif">>}, {name, <<"Tarifit"/utf8>>}, {name_en, <<"Tarifit"/utf8>>}]},
    {<<"lvs">>, [{language, <<"lvs">>}, {name, <<"Standard Latvian"/utf8>>}, {name_en, <<"Standard Latvian"/utf8>>}]},
    {<<"mlq">>, [{language, <<"mlq">>}, {name, <<"Western Maninkakan"/utf8>>}, {name_en, <<"Western Maninkakan"/utf8>>}]},
    {<<"lgg">>, [{language, <<"lgg">>}, {name, <<"Lugbara"/utf8>>}, {name_en, <<"Lugbara"/utf8>>}]},
    {<<"lrc">>, [{language, <<"lrc">>}, {name, <<"Northern Luri"/utf8>>}, {name_en, <<"Northern Luri"/utf8>>}]},
    {<<"quz">>, [{language, <<"quz">>}, {name, <<"Cusco Quechua"/utf8>>}, {name_en, <<"Cusco Quechua"/utf8>>}]},
    {<<"tcy">>, [{language, <<"tcy">>}, {name, <<"Tulu"/utf8>>}, {name_en, <<"Tulu"/utf8>>}]},
    {<<"xnr">>, [{language, <<"xnr">>}, {name, <<"Kangri"/utf8>>}, {name_en, <<"Kangri"/utf8>>}]},
    {<<"fuh">>, [{language, <<"fuh">>}, {name, <<"Western Niger Fulfulde"/utf8>>}, {name_en, <<"Western Niger Fulfulde"/utf8>>}]},
    {<<"xmw">>, [{language, <<"xmw">>}, {name, <<"Tsimihety Malagasy"/utf8>>}, {name_en, <<"Tsimihety Malagasy"/utf8>>}]},
    {<<"dnj">>, [{language, <<"dnj">>}, {name, <<"Dan"/utf8>>}, {name_en, <<"Dan"/utf8>>}]},
    {<<"kao">>, [{language, <<"kao">>}, {name, <<"Xaasongaxango"/utf8>>}, {name_en, <<"Xaasongaxango"/utf8>>}]},
    {<<"kbd">>, [{language, <<"kbd">>}, {name, <<"Kabardian"/utf8>>}, {name_en, <<"Kabardian"/utf8>>}]},
    {<<"kha">>, [{language, <<"kha">>}, {name, <<"Khasi"/utf8>>}, {name_en, <<"Khasi"/utf8>>}]},
    {<<"pms">>, [{language, <<"pms">>}, {name, <<"Piemontese"/utf8>>}, {name_en, <<"Piemontese"/utf8>>}]},
    {<<"pse">>, [{language, <<"pse">>}, {name, <<"Central Malay"/utf8>>}, {name_en, <<"Central Malay"/utf8>>}]},
    {<<"swk">>, [{language, <<"swk">>}, {name, <<"Malawi Sena"/utf8>>}, {name_en, <<"Malawi Sena"/utf8>>}]},
    {<<"tyz">>, [{language, <<"tyz">>}, {name, <<"Tày"/utf8>>}, {name_en, <<"Tày"/utf8>>}]},
    {<<"unr">>, [{language, <<"unr">>}, {name, <<"Mundari"/utf8>>}, {name_en, <<"Mundari"/utf8>>}]},
    {<<"wal">>, [{language, <<"wal">>}, {name, <<"Wolaytta"/utf8>>}, {name_en, <<"Wolaytta"/utf8>>}]},
    {<<"zza">>, [{language, <<"zza">>}, {name, <<"Zaza"/utf8>>}, {name_en, <<"Zaza"/utf8>>}]},
    {<<"zlj">>, [{language, <<"zlj">>}, {name, <<"Liujiang Zhuang"/utf8>>}, {name_en, <<"Liujiang Zhuang"/utf8>>}]},
    {<<"rmn">>, [{language, <<"rmn">>}, {name, <<"Balkan Romani"/utf8>>}, {name_en, <<"Balkan Romani"/utf8>>}]},
    {<<"sco">>, [{language, <<"sco">>}, {name, <<"Scots"/utf8>>}, {name_en, <<"Scots"/utf8>>}]},
    {<<"lub">>, [{language, <<"lub">>}, {name, <<"Tshiluba"/utf8>>}, {name_en, <<"Tshiluba"/utf8>>}]},
    {<<"ydd">>, [{language, <<"ydd">>}, {name, <<"Eastern Yiddish"/utf8>>}, {name_en, <<"Eastern Yiddish"/utf8>>}]},
    {<<"yid">>, [{language, <<"yid">>}, {name, <<"Yiddish"/utf8>>}, {name_en, <<"Yiddish"/utf8>>}]},
    {<<"ton">>, [{language, <<"ton">>}, {name, <<"Tonga"/utf8>>}, {name_en, <<"Tonga"/utf8>>}]},
    {<<"doh">>, [{language, <<"doh">>}, {name, <<"Dong"/utf8>>}, {name_en, <<"Dong"/utf8>>}]},
    {<<"dov">>, [{language, <<"dov">>}, {name, <<"Dombe"/utf8>>}, {name_en, <<"Dombe"/utf8>>}]},
    {<<"ibb">>, [{language, <<"ibb">>}, {name, <<"Ibibio"/utf8>>}, {name_en, <<"Ibibio"/utf8>>}]},
    {<<"kdx">>, [{language, <<"kdx">>}, {name, <<"Kam"/utf8>>}, {name_en, <<"Kam"/utf8>>}]},
    {<<"laj">>, [{language, <<"laj">>}, {name, <<"Lango (Uganda)"/utf8>>}, {name_en, <<"Lango (Uganda)"/utf8>>}]},
    {<<"lno">>, [{language, <<"lno">>}, {name, <<"Lango (Sudan)"/utf8>>}, {name_en, <<"Lango (Sudan)"/utf8>>}]},
    {<<"men">>, [{language, <<"men">>}, {name, <<"Mende (Sierra Leone)"/utf8>>}, {name_en, <<"Mende (Sierra Leone)"/utf8>>}]},
    {<<"ngl">>, [{language, <<"ngl">>}, {name, <<"Lomwe"/utf8>>}, {name_en, <<"Lomwe"/utf8>>}]},
    {<<"nne">>, [{language, <<"nne">>}, {name, <<"Ngandyera"/utf8>>}, {name_en, <<"Ngandyera"/utf8>>}]},
    {<<"sim">>, [{language, <<"sim">>}, {name, <<"Mende (Papua New Guinea)"/utf8>>}, {name_en, <<"Mende (Papua New Guinea)"/utf8>>}]},
    {<<"xtr">>, [{language, <<"xtr">>}, {name, <<"Early Tripuri"/utf8>>}, {name_en, <<"Early Tripuri"/utf8>>}]},
    {<<"zgb">>, [{language, <<"zgb">>}, {name, <<"Guibei Zhuang"/utf8>>}, {name_en, <<"Guibei Zhuang"/utf8>>}]},
    {<<"sgj">>, [{language, <<"sgj">>}, {name, <<"Surgujia"/utf8>>}, {name_en, <<"Surgujia"/utf8>>}]},
    {<<"tsg">>, [{language, <<"tsg">>}, {name, <<"Tausug"/utf8>>}, {name_en, <<"Tausug"/utf8>>}]},
    {<<"che">>, [{language, <<"che">>}, {name, <<"Chechen"/utf8>>}, {name_en, <<"Chechen"/utf8>>}]},
    {<<"nbl">>, [{language, <<"nbl">>}, {name, <<"South Ndebele"/utf8>>}, {name_en, <<"South Ndebele"/utf8>>}]},
    {<<"cqd">>, [{language, <<"cqd">>}, {name, <<"Chuanqiandian Cluster Miao"/utf8>>}, {name_en, <<"Chuanqiandian Cluster Miao"/utf8>>}]},
    {<<"dib">>, [{language, <<"dib">>}, {name, <<"South Central Dinka"/utf8>>}, {name_en, <<"South Central Dinka"/utf8>>}]},
    {<<"din">>, [{language, <<"din">>}, {name, <<"Dinka"/utf8>>}, {name_en, <<"Dinka"/utf8>>}]},
    {<<"gog">>, [{language, <<"gog">>}, {name, <<"Gogo"/utf8>>}, {name_en, <<"Gogo"/utf8>>}]},
    {<<"kde">>, [{language, <<"kde">>}, {name, <<"Makonde"/utf8>>}, {name_en, <<"Makonde"/utf8>>}]},
    {<<"khg">>, [{language, <<"khg">>}, {name, <<"Khams Tibetan"/utf8>>}, {name_en, <<"Khams Tibetan"/utf8>>}]},
    {<<"kxm">>, [{language, <<"kxm">>}, {name, <<"Northern Khmer"/utf8>>}, {name_en, <<"Northern Khmer"/utf8>>}]},
    {<<"njd">>, [{language, <<"njd">>}, {name, <<"Ndonde Hamba"/utf8>>}, {name_en, <<"Ndonde Hamba"/utf8>>}]},
    {<<"shy">>, [{language, <<"shy">>}, {name, <<"Tachawit"/utf8>>}, {name_en, <<"Tachawit"/utf8>>}]},
    {<<"seh">>, [{language, <<"seh">>}, {name, <<"Sena"/utf8>>}, {name_en, <<"Sena"/utf8>>}]},
    {<<"tge">>, [{language, <<"tge">>}, {name, <<"Eastern Gorkha Tamang"/utf8>>}, {name_en, <<"Eastern Gorkha Tamang"/utf8>>}]},
    {<<"toi">>, [{language, <<"toi">>}, {name, <<"Tonga (Zambia)"/utf8>>}, {name_en, <<"Tonga (Zambia)"/utf8>>}]},
    {<<"man">>, [{language, <<"man">>}, {name, <<"Mandingo"/utf8>>}, {name_en, <<"Mandingo"/utf8>>}]},
    {<<"lim">>, [{language, <<"lim">>}, {name, <<"Limburgan"/utf8>>}, {name_en, <<"Limburgan"/utf8>>}]},
    {<<"egl">>, [{language, <<"egl">>}, {name, <<"Emilian"/utf8>>}, {name_en, <<"Emilian"/utf8>>}]},
    {<<"hay">>, [{language, <<"hay">>}, {name, <<"Haya"/utf8>>}, {name_en, <<"Haya"/utf8>>}]},
    {<<"kpe">>, [{language, <<"kpe">>}, {name, <<"Kpelle"/utf8>>}, {name_en, <<"Kpelle"/utf8>>}]},
    {<<"mas">>, [{language, <<"mas">>}, {name, <<"Masai"/utf8>>}, {name_en, <<"Masai"/utf8>>}]},
    {<<"mnk">>, [{language, <<"mnk">>}, {name, <<"Mandinka"/utf8>>}, {name_en, <<"Mandinka"/utf8>>}]},
    {<<"tdx">>, [{language, <<"tdx">>}, {name, <<"Tandroy-Mahafaly Malagasy"/utf8>>}, {name_en, <<"Tandroy-Mahafaly Malagasy"/utf8>>}]},
    {<<"xpe">>, [{language, <<"xpe">>}, {name, <<"Liberia Kpelle"/utf8>>}, {name_en, <<"Liberia Kpelle"/utf8>>}]},
    {<<"hea">>, [{language, <<"hea">>}, {name, <<"Northern Qiandong Miao"/utf8>>}, {name_en, <<"Northern Qiandong Miao"/utf8>>}]},
    {<<"mni">>, [{language, <<"mni">>}, {name, <<"Manipuri"/utf8>>}, {name_en, <<"Manipuri"/utf8>>}]},
    {<<"zne">>, [{language, <<"zne">>}, {name, <<"Zande"/utf8>>}, {name_en, <<"Zande"/utf8>>}]},
    {<<"taj">>, [{language, <<"taj">>}, {name, <<"Eastern Tamang"/utf8>>}, {name_en, <<"Eastern Tamang"/utf8>>}]},
    {<<"skg">>, [{language, <<"skg">>}, {name, <<"Sakalava Malagasy"/utf8>>}, {name_en, <<"Sakalava Malagasy"/utf8>>}]},
    {<<"bod">>, [{language, <<"bod">>}, {name, <<"Tibetan"/utf8>>}, {name_en, <<"Tibetan"/utf8>>}]},
    {<<"luy">>, [{language, <<"luy">>}, {name, <<"Luyia"/utf8>>}, {name_en, <<"Luyia"/utf8>>}]},
    {<<"mtb">>, [{language, <<"mtb">>}, {name, <<"Anyin Morofo"/utf8>>}, {name_en, <<"Anyin Morofo"/utf8>>}]},
    {<<"nle">>, [{language, <<"nle">>}, {name, <<"East Nyala"/utf8>>}, {name_en, <<"East Nyala"/utf8>>}]},
    {<<"nyk">>, [{language, <<"nyk">>}, {name, <<"Nyaneka"/utf8>>}, {name_en, <<"Nyaneka"/utf8>>}]},
    {<<"nyy">>, [{language, <<"nyy">>}, {name, <<"Nyakyusa-Ngonde"/utf8>>}, {name_en, <<"Nyakyusa-Ngonde"/utf8>>}]},
    {<<"ola">>, [{language, <<"ola">>}, {name, <<"Walungge"/utf8>>}, {name_en, <<"Walungge"/utf8>>}]},
    {<<"pag">>, [{language, <<"pag">>}, {name, <<"Pangasinan"/utf8>>}, {name_en, <<"Pangasinan"/utf8>>}]},
    {<<"pnt">>, [{language, <<"pnt">>}, {name, <<"Pontic"/utf8>>}, {name_en, <<"Pontic"/utf8>>}]},
    {<<"sjp">>, [{language, <<"sjp">>}, {name, <<"Surjapuri"/utf8>>}, {name_en, <<"Surjapuri"/utf8>>}]},
    {<<"srr">>, [{language, <<"srr">>}, {name, <<"Serer"/utf8>>}, {name_en, <<"Serer"/utf8>>}]},
    {<<"tem">>, [{language, <<"tem">>}, {name, <<"Timne"/utf8>>}, {name_en, <<"Timne"/utf8>>}]},
    {<<"vas">>, [{language, <<"vas">>}, {name, <<"Vasavi"/utf8>>}, {name_en, <<"Vasavi"/utf8>>}]},
    {<<"zeh">>, [{language, <<"zeh">>}, {name, <<"Eastern Hongshuihe Zhuang"/utf8>>}, {name_en, <<"Eastern Hongshuihe Zhuang"/utf8>>}]},
    {<<"dag">>, [{language, <<"dag">>}, {name, <<"Dagbani"/utf8>>}, {name_en, <<"Dagbani"/utf8>>}]},
    {<<"mkw">>, [{language, <<"mkw">>}, {name, <<"Kituba (Congo)"/utf8>>}, {name_en, <<"Kituba (Congo)"/utf8>>}]},
    {<<"ekk">>, [{language, <<"ekk">>}, {name, <<"Standard Estonian"/utf8>>}, {name_en, <<"Standard Estonian"/utf8>>}]},
    {<<"tkg">>, [{language, <<"tkg">>}, {name, <<"Tesaka Malagasy"/utf8>>}, {name_en, <<"Tesaka Malagasy"/utf8>>}]},
    {<<"chv">>, [{language, <<"chv">>}, {name, <<"Chuvash"/utf8>>}, {name_en, <<"Chuvash"/utf8>>}]},
    {<<"dga">>, [{language, <<"dga">>}, {name, <<"Southern Dagaare"/utf8>>}, {name_en, <<"Southern Dagaare"/utf8>>}]},
    {<<"mdh">>, [{language, <<"mdh">>}, {name, <<"Maguindanaon"/utf8>>}, {name_en, <<"Maguindanaon"/utf8>>}]},
    {<<"mtq">>, [{language, <<"mtq">>}, {name, <<"Muong"/utf8>>}, {name_en, <<"Muong"/utf8>>}]},
    {<<"rgn">>, [{language, <<"rgn">>}, {name, <<"Romagnol"/utf8>>}, {name_en, <<"Romagnol"/utf8>>}]},
    {<<"shu">>, [{language, <<"shu">>}, {name, <<"Chadian Arabic"/utf8>>}, {name_en, <<"Chadian Arabic"/utf8>>}]},
    {<<"zch">>, [{language, <<"zch">>}, {name, <<"Central Hongshuihe Zhuang"/utf8>>}, {name_en, <<"Central Hongshuihe Zhuang"/utf8>>}]},
    {<<"sus">>, [{language, <<"sus">>}, {name, <<"Susu"/utf8>>}, {name_en, <<"Susu"/utf8>>}]},
    {<<"tig">>, [{language, <<"tig">>}, {name, <<"Tigre"/utf8>>}, {name_en, <<"Tigre"/utf8>>}]},
    {<<"hoc">>, [{language, <<"hoc">>}, {name, <<"Ho"/utf8>>}, {name_en, <<"Ho"/utf8>>}]},
    {<<"nga">>, [{language, <<"nga">>}, {name, <<"Ngbaka"/utf8>>}, {name_en, <<"Ngbaka"/utf8>>}]},
    {<<"tsc">>, [{language, <<"tsc">>}, {name, <<"Tswa"/utf8>>}, {name_en, <<"Tswa"/utf8>>}]},
    {<<"srd">>, [{language, <<"srd">>}, {name, <<"Sardinian"/utf8>>}, {name_en, <<"Sardinian"/utf8>>}]},
    {<<"diq">>, [{language, <<"diq">>}, {name, <<"Dimli"/utf8>>}, {name_en, <<"Dimli"/utf8>>}]},
    {<<"ffm">>, [{language, <<"ffm">>}, {name, <<"Maasina Fulfulde"/utf8>>}, {name_en, <<"Maasina Fulfulde"/utf8>>}]},
    {<<"gor">>, [{language, <<"gor">>}, {name, <<"Gorontalo"/utf8>>}, {name_en, <<"Gorontalo"/utf8>>}]},
    {<<"grt">>, [{language, <<"grt">>}, {name, <<"Garo"/utf8>>}, {name_en, <<"Garo"/utf8>>}]},
    {<<"igb">>, [{language, <<"igb">>}, {name, <<"Ebira"/utf8>>}, {name_en, <<"Ebira"/utf8>>}]},
    {<<"ijc">>, [{language, <<"ijc">>}, {name, <<"Izon"/utf8>>}, {name_en, <<"Izon"/utf8>>}]},
    {<<"jax">>, [{language, <<"jax">>}, {name, <<"Jambi Malay"/utf8>>}, {name_en, <<"Jambi Malay"/utf8>>}]},
    {<<"kbp">>, [{language, <<"kbp">>}, {name, <<"Kabiyè"/utf8>>}, {name_en, <<"Kabiyè"/utf8>>}]},
    {<<"kjp">>, [{language, <<"kjp">>}, {name, <<"Pwo Eastern Karen"/utf8>>}, {name_en, <<"Pwo Eastern Karen"/utf8>>}]},
    {<<"kmc">>, [{language, <<"kmc">>}, {name, <<"Southern Dong"/utf8>>}, {name_en, <<"Southern Dong"/utf8>>}]},
    {<<"kmz">>, [{language, <<"kmz">>}, {name, <<"Khorasani Turkish"/utf8>>}, {name_en, <<"Khorasani Turkish"/utf8>>}]},
    {<<"nch">>, [{language, <<"nch">>}, {name, <<"Central Huasteca Nahuatl"/utf8>>}, {name_en, <<"Central Huasteca Nahuatl"/utf8>>}]},
    {<<"nhe">>, [{language, <<"nhe">>}, {name, <<"Eastern Huasteca Nahuatl"/utf8>>}, {name_en, <<"Eastern Huasteca Nahuatl"/utf8>>}]},
    {<<"nhw">>, [{language, <<"nhw">>}, {name, <<"Western Huasteca Nahuatl"/utf8>>}, {name_en, <<"Western Huasteca Nahuatl"/utf8>>}]},
    {<<"nym">>, [{language, <<"nym">>}, {name, <<"Nyamwezi"/utf8>>}, {name_en, <<"Nyamwezi"/utf8>>}]},
    {<<"rob">>, [{language, <<"rob">>}, {name, <<"Tae'"/utf8>>}, {name_en, <<"Tae'"/utf8>>}]},
    {<<"sop">>, [{language, <<"sop">>}, {name, <<"Songe"/utf8>>}, {name_en, <<"Songe"/utf8>>}]},
    {<<"sth">>, [{language, <<"sth">>}, {name, <<"Shelta"/utf8>>}, {name_en, <<"Shelta"/utf8>>}]},
    {<<"fub">>, [{language, <<"fub">>}, {name, <<"Adamawa Fulfulde"/utf8>>}, {name_en, <<"Adamawa Fulfulde"/utf8>>}]},
    {<<"haq">>, [{language, <<"haq">>}, {name, <<"Ha"/utf8>>}, {name_en, <<"Ha"/utf8>>}]},
    {<<"tuv">>, [{language, <<"tuv">>}, {name, <<"Turkana"/utf8>>}, {name_en, <<"Turkana"/utf8>>}]},
    {<<"drs">>, [{language, <<"drs">>}, {name, <<"Gedeo"/utf8>>}, {name_en, <<"Gedeo"/utf8>>}]},
    {<<"ryu">>, [{language, <<"ryu">>}, {name, <<"Central Okinawan"/utf8>>}, {name_en, <<"Central Okinawan"/utf8>>}]},
    {<<"nut">>, [{language, <<"nut">>}, {name, <<"Nung (Viet Nam)"/utf8>>}, {name_en, <<"Nung (Viet Nam)"/utf8>>}]},
    {<<"mxc">>, [{language, <<"mxc">>}, {name, <<"Manyika"/utf8>>}, {name_en, <<"Manyika"/utf8>>}]},
    {<<"kck">>, [{language, <<"kck">>}, {name, <<"Kalanga"/utf8>>}, {name_en, <<"Kalanga"/utf8>>}]},
    {<<"niq">>, [{language, <<"niq">>}, {name, <<"Nandi"/utf8>>}, {name_en, <<"Nandi"/utf8>>}]},
    {<<"nmq">>, [{language, <<"nmq">>}, {name, <<"Nambya"/utf8>>}, {name_en, <<"Nambya"/utf8>>}]},
    {<<"sba">>, [{language, <<"sba">>}, {name, <<"Ngambay"/utf8>>}, {name_en, <<"Ngambay"/utf8>>}]},
    {<<"kac">>, [{language, <<"kac">>}, {name, <<"Kachin"/utf8>>}, {name_en, <<"Kachin"/utf8>>}]},
    {<<"lis">>, [{language, <<"lis">>}, {name, <<"Lisu"/utf8>>}, {name_en, <<"Lisu"/utf8>>}]},
    {<<"stv">>, [{language, <<"stv">>}, {name, <<"Silt'e"/utf8>>}, {name_en, <<"Silt'e"/utf8>>}]},
    {<<"qxq">>, [{language, <<"qxq">>}, {name, <<"Qashqa'i"/utf8>>}, {name_en, <<"Qashqa'i"/utf8>>}]},
    {<<"qug">>, [{language, <<"qug">>}, {name, <<"Chimborazo Highland Quichua"/utf8>>}, {name_en, <<"Chimborazo Highland Quichua"/utf8>>}]},
    {<<"kxu">>, [{language, <<"kxu">>}, {name, <<"Kui (India)"/utf8>>}, {name_en, <<"Kui (India)"/utf8>>}]},
    {<<"qwh">>, [{language, <<"qwh">>}, {name, <<"Huaylas Ancash Quechua"/utf8>>}, {name_en, <<"Huaylas Ancash Quechua"/utf8>>}]},
    {<<"rmq">>, [{language, <<"rmq">>}, {name, <<"Caló"/utf8>>}, {name_en, <<"Caló"/utf8>>}]},
    {<<"fvr">>, [{language, <<"fvr">>}, {name, <<"Fur"/utf8>>}, {name_en, <<"Fur"/utf8>>}]},
    {<<"gju">>, [{language, <<"gju">>}, {name, <<"Gujari"/utf8>>}, {name_en, <<"Gujari"/utf8>>}]},
    {<<"gux">>, [{language, <<"gux">>}, {name, <<"Gourmanchéma"/utf8>>}, {name_en, <<"Gourmanchéma"/utf8>>}]},
    {<<"iyx">>, [{language, <<"iyx">>}, {name, <<"Yaka (Congo)"/utf8>>}, {name_en, <<"Yaka (Congo)"/utf8>>}]},
    {<<"luz">>, [{language, <<"luz">>}, {name, <<"Southern Luri"/utf8>>}, {name_en, <<"Southern Luri"/utf8>>}]},
    {<<"mjl">>, [{language, <<"mjl">>}, {name, <<"Mandeali"/utf8>>}, {name_en, <<"Mandeali"/utf8>>}]},
    {<<"mqy">>, [{language, <<"mqy">>}, {name, <<"Manggarai"/utf8>>}, {name_en, <<"Manggarai"/utf8>>}]},
    {<<"nnb">>, [{language, <<"nnb">>}, {name, <<"Nande"/utf8>>}, {name_en, <<"Nande"/utf8>>}]},
    {<<"old">>, [{language, <<"old">>}, {name, <<"Mochi"/utf8>>}, {name_en, <<"Mochi"/utf8>>}]},
    {<<"ppp">>, [{language, <<"ppp">>}, {name, <<"Pelende"/utf8>>}, {name_en, <<"Pelende"/utf8>>}]},
    {<<"quy">>, [{language, <<"quy">>}, {name, <<"Ayacucho Quechua"/utf8>>}, {name_en, <<"Ayacucho Quechua"/utf8>>}]},
    {<<"tly">>, [{language, <<"tly">>}, {name, <<"Talysh"/utf8>>}, {name_en, <<"Talysh"/utf8>>}]},
    {<<"vwa">>, [{language, <<"vwa">>}, {name, <<"Awa (China)"/utf8>>}, {name_en, <<"Awa (China)"/utf8>>}]},
    {<<"wbm">>, [{language, <<"wbm">>}, {name, <<"Wa"/utf8>>}, {name_en, <<"Wa"/utf8>>}]},
    {<<"yaf">>, [{language, <<"yaf">>}, {name, <<"Yaka (Democratic Republic of Congo)"/utf8>>}, {name_en, <<"Yaka (Democratic Republic of Congo)"/utf8>>}]},
    {<<"ktb">>, [{language, <<"ktb">>}, {name, <<"Kambaata"/utf8>>}, {name_en, <<"Kambaata"/utf8>>}]},
    {<<"nij">>, [{language, <<"nij">>}, {name, <<"Ngaju"/utf8>>}, {name_en, <<"Ngaju"/utf8>>}]},
    {<<"nus">>, [{language, <<"nus">>}, {name, <<"Nuer"/utf8>>}, {name_en, <<"Nuer"/utf8>>}]},
    {<<"kfr">>, [{language, <<"kfr">>}, {name, <<"Kachhi"/utf8>>}, {name_en, <<"Kachhi"/utf8>>}]},
    {<<"pht">>, [{language, <<"pht">>}, {name, <<"Phu Thai"/utf8>>}, {name_en, <<"Phu Thai"/utf8>>}]},
    {<<"zyj">>, [{language, <<"zyj">>}, {name, <<"Youjiang Zhuang"/utf8>>}, {name_en, <<"Youjiang Zhuang"/utf8>>}]},
    {<<"new">>, [{language, <<"new">>}, {name, <<"Newari"/utf8>>}, {name_en, <<"Newari"/utf8>>}]},
    {<<"nwx">>, [{language, <<"nwx">>}, {name, <<"Middle Newar"/utf8>>}, {name_en, <<"Middle Newar"/utf8>>}]},
    {<<"sef">>, [{language, <<"sef">>}, {name, <<"Cebaara Senoufo"/utf8>>}, {name_en, <<"Cebaara Senoufo"/utf8>>}]},
    {<<"mnw">>, [{language, <<"mnw">>}, {name, <<"Mon"/utf8>>}, {name_en, <<"Mon"/utf8>>}]},
    {<<"lon">>, [{language, <<"lon">>}, {name, <<"Malawi Lomwe"/utf8>>}, {name_en, <<"Malawi Lomwe"/utf8>>}]},
    {<<"xmm">>, [{language, <<"xmm">>}, {name, <<"Manado Malay"/utf8>>}, {name_en, <<"Manado Malay"/utf8>>}]},
    {<<"ium">>, [{language, <<"ium">>}, {name, <<"Iu Mein"/utf8>>}, {name_en, <<"Iu Mein"/utf8>>}]},
    {<<"kbr">>, [{language, <<"kbr">>}, {name, <<"Kafa"/utf8>>}, {name_en, <<"Kafa"/utf8>>}]},
    {<<"ljp">>, [{language, <<"ljp">>}, {name, <<"Lampung Api"/utf8>>}, {name_en, <<"Lampung Api"/utf8>>}]},
    {<<"mmr">>, [{language, <<"mmr">>}, {name, <<"Western Xiangxi Miao"/utf8>>}, {name_en, <<"Western Xiangxi Miao"/utf8>>}]},
    {<<"swg">>, [{language, <<"swg">>}, {name, <<"Swabian"/utf8>>}, {name_en, <<"Swabian"/utf8>>}]},
    {<<"ndo">>, [{language, <<"ndo">>}, {name, <<"Ndonga"/utf8>>}, {name_en, <<"Ndonga"/utf8>>}]},
    {<<"heh">>, [{language, <<"heh">>}, {name, <<"Hehe"/utf8>>}, {name_en, <<"Hehe"/utf8>>}]},
    {<<"prk">>, [{language, <<"prk">>}, {name, <<"Parauk"/utf8>>}, {name_en, <<"Parauk"/utf8>>}]},
    {<<"oci">>, [{language, <<"oci">>}, {name, <<"Occitan"/utf8>>}, {name_en, <<"Occitan"/utf8>>}]},
    {<<"bfr">>, [{language, <<"bfr">>}, {name, <<"Bazigar"/utf8>>}, {name_en, <<"Bazigar"/utf8>>}]},
    {<<"igl">>, [{language, <<"igl">>}, {name, <<"Igala"/utf8>>}, {name_en, <<"Igala"/utf8>>}]},
    {<<"juy">>, [{language, <<"juy">>}, {name, <<"Juray"/utf8>>}, {name_en, <<"Juray"/utf8>>}]},
    {<<"kek">>, [{language, <<"kek">>}, {name, <<"Kekchí"/utf8>>}, {name_en, <<"Kekchí"/utf8>>}]},
    {<<"mfe">>, [{language, <<"mfe">>}, {name, <<"Morisyen"/utf8>>}, {name_en, <<"Morisyen"/utf8>>}]},
    {<<"mgh">>, [{language, <<"mgh">>}, {name, <<"Makhuwa-Meetto"/utf8>>}, {name_en, <<"Makhuwa-Meetto"/utf8>>}]},
    {<<"nqg">>, [{language, <<"nqg">>}, {name, <<"Southern Nago"/utf8>>}, {name_en, <<"Southern Nago"/utf8>>}]},
    {<<"nqk">>, [{language, <<"nqk">>}, {name, <<"Kura Ede Nago"/utf8>>}, {name_en, <<"Kura Ede Nago"/utf8>>}]},
    {<<"nup">>, [{language, <<"nup">>}, {name, <<"Nupe-Nupe-Tako"/utf8>>}, {name_en, <<"Nupe-Nupe-Tako"/utf8>>}]},
    {<<"iba">>, [{language, <<"iba">>}, {name, <<"Iban"/utf8>>}, {name_en, <<"Iban"/utf8>>}]},
    {<<"yua">>, [{language, <<"yua">>}, {name, <<"Yucateco"/utf8>>}, {name_en, <<"Yucateco"/utf8>>}]},
    {<<"dty">>, [{language, <<"dty">>}, {name, <<"Dotyali"/utf8>>}, {name_en, <<"Dotyali"/utf8>>}]},
    {<<"mrw">>, [{language, <<"mrw">>}, {name, <<"Maranao"/utf8>>}, {name_en, <<"Maranao"/utf8>>}]},
    {<<"trp">>, [{language, <<"trp">>}, {name, <<"Kok Borok"/utf8>>}, {name_en, <<"Kok Borok"/utf8>>}]},
    {<<"nia">>, [{language, <<"nia">>}, {name, <<"Nias"/utf8>>}, {name_en, <<"Nias"/utf8>>}]},
    {<<"zyg">>, [{language, <<"zyg">>}, {name, <<"Yang Zhuang"/utf8>>}, {name_en, <<"Yang Zhuang"/utf8>>}]},
    {<<"hni">>, [{language, <<"hni">>}, {name, <<"Hani"/utf8>>}, {name_en, <<"Hani"/utf8>>}]},
    {<<"led">>, [{language, <<"led">>}, {name, <<"Lendu"/utf8>>}, {name_en, <<"Lendu"/utf8>>}]},
    {<<"sda">>, [{language, <<"sda">>}, {name, <<"Toraja-Sa'dan"/utf8>>}, {name_en, <<"Toraja-Sa'dan"/utf8>>}]},
    {<<"tll">>, [{language, <<"tll">>}, {name, <<"Tetela"/utf8>>}, {name_en, <<"Tetela"/utf8>>}]},
    {<<"gaa">>, [{language, <<"gaa">>}, {name, <<"Ga"/utf8>>}, {name_en, <<"Ga"/utf8>>}]},
    {<<"kdj">>, [{language, <<"kdj">>}, {name, <<"Karamojong"/utf8>>}, {name_en, <<"Karamojong"/utf8>>}]},
    {<<"myk">>, [{language, <<"myk">>}, {name, <<"Mamara Senoufo"/utf8>>}, {name_en, <<"Mamara Senoufo"/utf8>>}]},
    {<<"txy">>, [{language, <<"txy">>}, {name, <<"Tanosy Malagasy"/utf8>>}, {name_en, <<"Tanosy Malagasy"/utf8>>}]},
    {<<"yom">>, [{language, <<"yom">>}, {name, <<"Yombe"/utf8>>}, {name_en, <<"Yombe"/utf8>>}]},
    {<<"loz">>, [{language, <<"loz">>}, {name, <<"Lozi"/utf8>>}, {name_en, <<"Lozi"/utf8>>}]},
    {<<"gur">>, [{language, <<"gur">>}, {name, <<"Farefare"/utf8>>}, {name_en, <<"Farefare"/utf8>>}]},
    {<<"rng">>, [{language, <<"rng">>}, {name, <<"Ronga"/utf8>>}, {name_en, <<"Ronga"/utf8>>}]},
    {<<"tdd">>, [{language, <<"tdd">>}, {name, <<"Tai Nüa"/utf8>>}, {name_en, <<"Tai Nüa"/utf8>>}]},
    {<<"ava">>, [{language, <<"ava">>}, {name, <<"Avaric"/utf8>>}, {name_en, <<"Avaric"/utf8>>}]},
    {<<"kjg">>, [{language, <<"kjg">>}, {name, <<"Khmu"/utf8>>}, {name_en, <<"Khmu"/utf8>>}]},
    {<<"gbr">>, [{language, <<"gbr">>}, {name, <<"Gbagyi"/utf8>>}, {name_en, <<"Gbagyi"/utf8>>}]},
    {<<"gyn">>, [{language, <<"gyn">>}, {name, <<"Guyanese Creole English"/utf8>>}, {name_en, <<"Guyanese Creole English"/utf8>>}]},
    {<<"lic">>, [{language, <<"lic">>}, {name, <<"Hlai"/utf8>>}, {name_en, <<"Hlai"/utf8>>}]},
    {<<"max">>, [{language, <<"max">>}, {name, <<"North Moluccan Malay"/utf8>>}, {name_en, <<"North Moluccan Malay"/utf8>>}]},
    {<<"pcd">>, [{language, <<"pcd">>}, {name, <<"Picard"/utf8>>}, {name_en, <<"Picard"/utf8>>}]},
    {<<"pko">>, [{language, <<"pko">>}, {name, <<"Pökoot"/utf8>>}, {name_en, <<"Pökoot"/utf8>>}]},
    {<<"kuj">>, [{language, <<"kuj">>}, {name, <<"Kuria"/utf8>>}, {name_en, <<"Kuria"/utf8>>}]},
    {<<"lus">>, [{language, <<"lus">>}, {name, <<"Lushai"/utf8>>}, {name_en, <<"Lushai"/utf8>>}]},
    {<<"ruf">>, [{language, <<"ruf">>}, {name, <<"Luguru"/utf8>>}, {name_en, <<"Luguru"/utf8>>}]},
    {<<"thl">>, [{language, <<"thl">>}, {name, <<"Dangaura Tharu"/utf8>>}, {name_en, <<"Dangaura Tharu"/utf8>>}]},
    {<<"nih">>, [{language, <<"nih">>}, {name, <<"Nyiha (Tanzania)"/utf8>>}, {name_en, <<"Nyiha (Tanzania)"/utf8>>}]},
    {<<"nyo">>, [{language, <<"nyo">>}, {name, <<"Nyoro"/utf8>>}, {name_en, <<"Nyoro"/utf8>>}]},
    {<<"ksb">>, [{language, <<"ksb">>}, {name, <<"Shambala"/utf8>>}, {name_en, <<"Shambala"/utf8>>}]},
    {<<"nyg">>, [{language, <<"nyg">>}, {name, <<"Nyindu"/utf8>>}, {name_en, <<"Nyindu"/utf8>>}]},
    {<<"shr">>, [{language, <<"shr">>}, {name, <<"Shi"/utf8>>}, {name_en, <<"Shi"/utf8>>}]},
    {<<"kua">>, [{language, <<"kua">>}, {name, <<"Kuanyama"/utf8>>}, {name_en, <<"Kuanyama"/utf8>>}]},
    {<<"bfb">>, [{language, <<"bfb">>}, {name, <<"Pauri Bareli"/utf8>>}, {name_en, <<"Pauri Bareli"/utf8>>}]},
    {<<"cps">>, [{language, <<"cps">>}, {name, <<"Capiznon"/utf8>>}, {name_en, <<"Capiznon"/utf8>>}]},
    {<<"lue">>, [{language, <<"lue">>}, {name, <<"Luvale"/utf8>>}, {name_en, <<"Luvale"/utf8>>}]},
    {<<"ttq">>, [{language, <<"ttq">>}, {name, <<"Tawallammat Tamajaq"/utf8>>}, {name_en, <<"Tawallammat Tamajaq"/utf8>>}]},
    {<<"hnd">>, [{language, <<"hnd">>}, {name, <<"Southern Hindko"/utf8>>}, {name_en, <<"Southern Hindko"/utf8>>}]},
    {<<"mdj">>, [{language, <<"mdj">>}, {name, <<"Mangbetu"/utf8>>}, {name_en, <<"Mangbetu"/utf8>>}]},
    {<<"rag">>, [{language, <<"rag">>}, {name, <<"Logooli"/utf8>>}, {name_en, <<"Logooli"/utf8>>}]},
    {<<"lez">>, [{language, <<"lez">>}, {name, <<"Lezghian"/utf8>>}, {name_en, <<"Lezghian"/utf8>>}]},
    {<<"fia">>, [{language, <<"fia">>}, {name, <<"Nobiin"/utf8>>}, {name_en, <<"Nobiin"/utf8>>}]},
    {<<"koo">>, [{language, <<"koo">>}, {name, <<"Konzo"/utf8>>}, {name_en, <<"Konzo"/utf8>>}]},
    {<<"wln">>, [{language, <<"wln">>}, {name, <<"Walloon"/utf8>>}, {name_en, <<"Walloon"/utf8>>}]},
    {<<"hlb">>, [{language, <<"hlb">>}, {name, <<"Halbi"/utf8>>}, {name_en, <<"Halbi"/utf8>>}]},
    {<<"hwc">>, [{language, <<"hwc">>}, {name, <<"Hawaiian Creole English"/utf8>>}, {name_en, <<"Hawaiian Creole English"/utf8>>}]},
    {<<"ida">>, [{language, <<"ida">>}, {name, <<"Idakho-Isukha-Tiriki"/utf8>>}, {name_en, <<"Idakho-Isukha-Tiriki"/utf8>>}]},
    {<<"idu">>, [{language, <<"idu">>}, {name, <<"Idoma"/utf8>>}, {name_en, <<"Idoma"/utf8>>}]},
    {<<"nse">>, [{language, <<"nse">>}, {name, <<"Nsenga"/utf8>>}, {name_en, <<"Nsenga"/utf8>>}]},
    {<<"onb">>, [{language, <<"onb">>}, {name, <<"Lingao"/utf8>>}, {name_en, <<"Lingao"/utf8>>}]},
    {<<"rim">>, [{language, <<"rim">>}, {name, <<"Nyaturu"/utf8>>}, {name_en, <<"Nyaturu"/utf8>>}]},
    {<<"vav">>, [{language, <<"vav">>}, {name, <<"Varli"/utf8>>}, {name_en, <<"Varli"/utf8>>}]},
    {<<"xon">>, [{language, <<"xon">>}, {name, <<"Konkomba"/utf8>>}, {name_en, <<"Konkomba"/utf8>>}]},
    {<<"qxp">>, [{language, <<"qxp">>}, {name, <<"Puno Quechua"/utf8>>}, {name_en, <<"Puno Quechua"/utf8>>}]},
    {<<"gos">>, [{language, <<"gos">>}, {name, <<"Gronings"/utf8>>}, {name_en, <<"Gronings"/utf8>>}]},
    {<<"eza">>, [{language, <<"eza">>}, {name, <<"Ezaa"/utf8>>}, {name_en, <<"Ezaa"/utf8>>}]},
    {<<"gag">>, [{language, <<"gag">>}, {name, <<"Gagauz"/utf8>>}, {name_en, <<"Gagauz"/utf8>>}]},
    {<<"fan">>, [{language, <<"fan">>}, {name, <<"Fang (Equatorial Guinea)"/utf8>>}, {name_en, <<"Fang (Equatorial Guinea)"/utf8>>}]},
    {<<"kaa">>, [{language, <<"kaa">>}, {name, <<"Kara-Kalpak"/utf8>>}, {name_en, <<"Kara-Kalpak"/utf8>>}]},
    {<<"ewo">>, [{language, <<"ewo">>}, {name, <<"Ewondo"/utf8>>}, {name_en, <<"Ewondo"/utf8>>}]},
    {<<"nue">>, [{language, <<"nue">>}, {name, <<"Ngundu"/utf8>>}, {name_en, <<"Ngundu"/utf8>>}]},
    {<<"oss">>, [{language, <<"oss">>}, {name, <<"Ossetian"/utf8>>}, {name_en, <<"Ossetian"/utf8>>}]},
    {<<"ijn">>, [{language, <<"ijn">>}, {name, <<"Kalabari"/utf8>>}, {name_en, <<"Kalabari"/utf8>>}]},
    {<<"kfq">>, [{language, <<"kfq">>}, {name, <<"Korku"/utf8>>}, {name_en, <<"Korku"/utf8>>}]},
    {<<"okr">>, [{language, <<"okr">>}, {name, <<"Kirike"/utf8>>}, {name_en, <<"Kirike"/utf8>>}]},
    {<<"ycl">>, [{language, <<"ycl">>}, {name, <<"Lolopo"/utf8>>}, {name_en, <<"Lolopo"/utf8>>}]},
    {<<"ysp">>, [{language, <<"ysp">>}, {name, <<"Southern Lolopo"/utf8>>}, {name_en, <<"Southern Lolopo"/utf8>>}]},
    {<<"rcf">>, [{language, <<"rcf">>}, {name, <<"Reunion Creole"/utf8>>}, {name_en, <<"Reunion Creole"/utf8>>}]},
    {<<"udm">>, [{language, <<"udm">>}, {name, <<"Udmurt"/utf8>>}, {name_en, <<"Udmurt"/utf8>>}]},
    {<<"khb">>, [{language, <<"khb">>}, {name, <<"Lü"/utf8>>}, {name_en, <<"Lü"/utf8>>}]},
    {<<"msh">>, [{language, <<"msh">>}, {name, <<"Masikoro Malagasy"/utf8>>}, {name_en, <<"Masikoro Malagasy"/utf8>>}]},
    {<<"urh">>, [{language, <<"urh">>}, {name, <<"Urhobo"/utf8>>}, {name_en, <<"Urhobo"/utf8>>}]},
    {<<"dwr">>, [{language, <<"dwr">>}, {name, <<"Dawro"/utf8>>}, {name_en, <<"Dawro"/utf8>>}]},
    {<<"jrb">>, [{language, <<"jrb">>}, {name, <<"Judeo-Arabic"/utf8>>}, {name_en, <<"Judeo-Arabic"/utf8>>}]},
    {<<"kxp">>, [{language, <<"kxp">>}, {name, <<"Wadiyara Koli"/utf8>>}, {name_en, <<"Wadiyara Koli"/utf8>>}]},
    {<<"izz">>, [{language, <<"izz">>}, {name, <<"Izii"/utf8>>}, {name_en, <<"Izii"/utf8>>}]},
    {<<"mam">>, [{language, <<"mam">>}, {name, <<"Mam"/utf8>>}, {name_en, <<"Mam"/utf8>>}]},
    {<<"mrg">>, [{language, <<"mrg">>}, {name, <<"Mising"/utf8>>}, {name_en, <<"Mising"/utf8>>}]},
    {<<"kwy">>, [{language, <<"kwy">>}, {name, <<"San Salvador Kongo"/utf8>>}, {name_en, <<"San Salvador Kongo"/utf8>>}]},
    {<<"mgp">>, [{language, <<"mgp">>}, {name, <<"Eastern Magar"/utf8>>}, {name_en, <<"Eastern Magar"/utf8>>}]},
    {<<"lhu">>, [{language, <<"lhu">>}, {name, <<"Lahu"/utf8>>}, {name_en, <<"Lahu"/utf8>>}]},
    {<<"mkc">>, [{language, <<"mkc">>}, {name, <<"Siliput"/utf8>>}, {name_en, <<"Siliput"/utf8>>}]},
    {<<"spv">>, [{language, <<"spv">>}, {name, <<"Sambalpuri"/utf8>>}, {name_en, <<"Sambalpuri"/utf8>>}]},
    {<<"xdy">>, [{language, <<"xdy">>}, {name, <<"Malayic Dayak"/utf8>>}, {name_en, <<"Malayic Dayak"/utf8>>}]},
    {<<"hav">>, [{language, <<"hav">>}, {name, <<"Havu"/utf8>>}, {name_en, <<"Havu"/utf8>>}]},
    {<<"its">>, [{language, <<"its">>}, {name, <<"Isekiri"/utf8>>}, {name_en, <<"Isekiri"/utf8>>}]},
    {<<"szl">>, [{language, <<"szl">>}, {name, <<"Silesian"/utf8>>}, {name_en, <<"Silesian"/utf8>>}]},
    {<<"zmi">>, [{language, <<"zmi">>}, {name, <<"Negeri Sembilan Malay"/utf8>>}, {name_en, <<"Negeri Sembilan Malay"/utf8>>}]},
    {<<"gjk">>, [{language, <<"gjk">>}, {name, <<"Kachi Koli"/utf8>>}, {name_en, <<"Kachi Koli"/utf8>>}]},
    {<<"goa">>, [{language, <<"goa">>}, {name, <<"Guro"/utf8>>}, {name_en, <<"Guro"/utf8>>}]},
    {<<"hms">>, [{language, <<"hms">>}, {name, <<"Southern Qiandong Miao"/utf8>>}, {name_en, <<"Southern Qiandong Miao"/utf8>>}]},
    {<<"ibg">>, [{language, <<"ibg">>}, {name, <<"Ibanag"/utf8>>}, {name_en, <<"Ibanag"/utf8>>}]},
    {<<"kri">>, [{language, <<"kri">>}, {name, <<"Krio"/utf8>>}, {name_en, <<"Krio"/utf8>>}]},
    {<<"lij">>, [{language, <<"lij">>}, {name, <<"Ligurian"/utf8>>}, {name_en, <<"Ligurian"/utf8>>}]},
    {<<"mgr">>, [{language, <<"mgr">>}, {name, <<"Mambwe-Lungu"/utf8>>}, {name_en, <<"Mambwe-Lungu"/utf8>>}]},
    {<<"ogc">>, [{language, <<"ogc">>}, {name, <<"Ogbah"/utf8>>}, {name_en, <<"Ogbah"/utf8>>}]},
    {<<"pmy">>, [{language, <<"pmy">>}, {name, <<"Papuan Malay"/utf8>>}, {name_en, <<"Papuan Malay"/utf8>>}]},
    {<<"ppt">>, [{language, <<"ppt">>}, {name, <<"Pare"/utf8>>}, {name_en, <<"Pare"/utf8>>}]},
    {<<"scl">>, [{language, <<"scl">>}, {name, <<"Shina"/utf8>>}, {name_en, <<"Shina"/utf8>>}]},
    {<<"sgs">>, [{language, <<"sgs">>}, {name, <<"Samogitian"/utf8>>}, {name_en, <<"Samogitian"/utf8>>}]},
    {<<"src">>, [{language, <<"src">>}, {name, <<"Logudorese Sardinian"/utf8>>}, {name_en, <<"Logudorese Sardinian"/utf8>>}]},
    {<<"sro">>, [{language, <<"sro">>}, {name, <<"Campidanese Sardinian"/utf8>>}, {name_en, <<"Campidanese Sardinian"/utf8>>}]},
    {<<"tet">>, [{language, <<"tet">>}, {name, <<"Tetum"/utf8>>}, {name_en, <<"Tetum"/utf8>>}]},
    {<<"twh">>, [{language, <<"twh">>}, {name, <<"Tai Dón"/utf8>>}, {name_en, <<"Tai Dón"/utf8>>}]},
    {<<"vmk">>, [{language, <<"vmk">>}, {name, <<"Makhuwa-Shirima"/utf8>>}, {name_en, <<"Makhuwa-Shirima"/utf8>>}]},
    {<<"xmf">>, [{language, <<"xmf">>}, {name, <<"Mingrelian"/utf8>>}, {name_en, <<"Mingrelian"/utf8>>}]},
    {<<"yig">>, [{language, <<"yig">>}, {name, <<"Wusa Nasu"/utf8>>}, {name_en, <<"Wusa Nasu"/utf8>>}]},
    {<<"zhn">>, [{language, <<"zhn">>}, {name, <<"Nong Zhuang"/utf8>>}, {name_en, <<"Nong Zhuang"/utf8>>}]},
    {<<"kea">>, [{language, <<"kea">>}, {name, <<"Kabuverdianu"/utf8>>}, {name_en, <<"Kabuverdianu"/utf8>>}]},
    {<<"ttj">>, [{language, <<"ttj">>}, {name, <<"Tooro"/utf8>>}, {name_en, <<"Tooro"/utf8>>}]},
    {<<"thr">>, [{language, <<"thr">>}, {name, <<"Rana Tharu"/utf8>>}, {name_en, <<"Rana Tharu"/utf8>>}]},
    {<<"rmy">>, [{language, <<"rmy">>}, {name, <<"Vlax Romani"/utf8>>}, {name_en, <<"Vlax Romani"/utf8>>}]},
    {<<"crh">>, [{language, <<"crh">>}, {name, <<"Crimean Tatar"/utf8>>}, {name_en, <<"Crimean Tatar"/utf8>>}]},
    {<<"drt">>, [{language, <<"drt">>}, {name, <<"Drents"/utf8>>}, {name_en, <<"Drents"/utf8>>}]},
    {<<"lsm">>, [{language, <<"lsm">>}, {name, <<"Saamia"/utf8>>}, {name_en, <<"Saamia"/utf8>>}]},
    {<<"mdr">>, [{language, <<"mdr">>}, {name, <<"Mandar"/utf8>>}, {name_en, <<"Mandar"/utf8>>}]},
    {<<"sah">>, [{language, <<"sah">>}, {name, <<"Yakut"/utf8>>}, {name_en, <<"Yakut"/utf8>>}]},
    {<<"wci">>, [{language, <<"wci">>}, {name, <<"Waci Gbe"/utf8>>}, {name_en, <<"Waci Gbe"/utf8>>}]},
    {<<"fry">>, [{language, <<"fry">>}, {name, <<"Western Frisian"/utf8>>}, {name_en, <<"Western Frisian"/utf8>>}]},
    {<<"kge">>, [{language, <<"kge">>}, {name, <<"Komering"/utf8>>}, {name_en, <<"Komering"/utf8>>}]},
    {<<"mgn">>, [{language, <<"mgn">>}, {name, <<"Mbangi"/utf8>>}, {name_en, <<"Mbangi"/utf8>>}]},
    {<<"mhr">>, [{language, <<"mhr">>}, {name, <<"Eastern Mari"/utf8>>}, {name_en, <<"Eastern Mari"/utf8>>}]},
    {<<"mjh">>, [{language, <<"mjh">>}, {name, <<"Mwera (Nyasa)"/utf8>>}, {name_en, <<"Mwera (Nyasa)"/utf8>>}]},
    {<<"mwe">>, [{language, <<"mwe">>}, {name, <<"Mwera (Chimwera)"/utf8>>}, {name_en, <<"Mwera (Chimwera)"/utf8>>}]},
    {<<"unx">>, [{language, <<"unx">>}, {name, <<"Munda"/utf8>>}, {name_en, <<"Munda"/utf8>>}]},
    {<<"rtw">>, [{language, <<"rtw">>}, {name, <<"Rathawi"/utf8>>}, {name_en, <<"Rathawi"/utf8>>}]},
    {<<"doc">>, [{language, <<"doc">>}, {name, <<"Northern Dong"/utf8>>}, {name_en, <<"Northern Dong"/utf8>>}]},
    {<<"gkp">>, [{language, <<"gkp">>}, {name, <<"Guinea Kpelle"/utf8>>}, {name_en, <<"Guinea Kpelle"/utf8>>}]},
    {<<"irk">>, [{language, <<"irk">>}, {name, <<"Iraqw"/utf8>>}, {name_en, <<"Iraqw"/utf8>>}]},
    {<<"kbl">>, [{language, <<"kbl">>}, {name, <<"Kanembu"/utf8>>}, {name_en, <<"Kanembu"/utf8>>}]},
    {<<"nim">>, [{language, <<"nim">>}, {name, <<"Nilamba"/utf8>>}, {name_en, <<"Nilamba"/utf8>>}]},
    {<<"dik">>, [{language, <<"dik">>}, {name, <<"Southwestern Dinka"/utf8>>}, {name_en, <<"Southwestern Dinka"/utf8>>}]},
    {<<"fuq">>, [{language, <<"fuq">>}, {name, <<"Central-Eastern Niger Fulfulde"/utf8>>}, {name_en, <<"Central-Eastern Niger Fulfulde"/utf8>>}]},
    {<<"kdt">>, [{language, <<"kdt">>}, {name, <<"Kuy"/utf8>>}, {name_en, <<"Kuy"/utf8>>}]},
    {<<"kum">>, [{language, <<"kum">>}, {name, <<"Kumyk"/utf8>>}, {name_en, <<"Kumyk"/utf8>>}]},
    {<<"mwk">>, [{language, <<"mwk">>}, {name, <<"Kita Maninkakan"/utf8>>}, {name_en, <<"Kita Maninkakan"/utf8>>}]},
    {<<"nyl">>, [{language, <<"nyl">>}, {name, <<"Nyeu"/utf8>>}, {name_en, <<"Nyeu"/utf8>>}]},
    {<<"pdt">>, [{language, <<"pdt">>}, {name, <<"Mennonite Low German"/utf8>>}, {name_en, <<"Mennonite Low German"/utf8>>}]},
    {<<"snj">>, [{language, <<"snj">>}, {name, <<"Riverain Sango"/utf8>>}, {name_en, <<"Riverain Sango"/utf8>>}]},
    {<<"tzh">>, [{language, <<"tzh">>}, {name, <<"Tzeltal"/utf8>>}, {name_en, <<"Tzeltal"/utf8>>}]},
    {<<"zap">>, [{language, <<"zap">>}, {name, <<"Zapotec"/utf8>>}, {name_en, <<"Zapotec"/utf8>>}]},
    {<<"mya">>, [{language, <<"mya">>}, {name, <<"Burmese"/utf8>>}, {name_en, <<"Burmese"/utf8>>}]},
    {<<"kus">>, [{language, <<"kus">>}, {name, <<"Kusaal"/utf8>>}, {name_en, <<"Kusaal"/utf8>>}]},
    {<<"lob">>, [{language, <<"lob">>}, {name, <<"Lobi"/utf8>>}, {name_en, <<"Lobi"/utf8>>}]},
    {<<"mfq">>, [{language, <<"mfq">>}, {name, <<"Moba"/utf8>>}, {name_en, <<"Moba"/utf8>>}]},
    {<<"nyu">>, [{language, <<"nyu">>}, {name, <<"Nyungwe"/utf8>>}, {name_en, <<"Nyungwe"/utf8>>}]},
    {<<"sgw">>, [{language, <<"sgw">>}, {name, <<"Sebat Bet Gurage"/utf8>>}, {name_en, <<"Sebat Bet Gurage"/utf8>>}]},
    {<<"tvn">>, [{language, <<"tvn">>}, {name, <<"Tavoyan"/utf8>>}, {name_en, <<"Tavoyan"/utf8>>}]},
    {<<"rej">>, [{language, <<"rej">>}, {name, <<"Rejang"/utf8>>}, {name_en, <<"Rejang"/utf8>>}]},
    {<<"smo">>, [{language, <<"smo">>}, {name, <<"Samoan"/utf8>>}, {name_en, <<"Samoan"/utf8>>}]},
    {<<"lch">>, [{language, <<"lch">>}, {name, <<"Luchazi"/utf8>>}, {name_en, <<"Luchazi"/utf8>>}]},
    {<<"gcf">>, [{language, <<"gcf">>}, {name, <<"Guadeloupean Creole French"/utf8>>}, {name_en, <<"Guadeloupean Creole French"/utf8>>}]},
    {<<"ses">>, [{language, <<"ses">>}, {name, <<"Koyraboro Senni Songhai"/utf8>>}, {name_en, <<"Koyraboro Senni Songhai"/utf8>>}]},
    {<<"sng">>, [{language, <<"sng">>}, {name, <<"Sanga (Democratic Republic of Congo)"/utf8>>}, {name_en, <<"Sanga (Democratic Republic of Congo)"/utf8>>}]},
    {<<"pwr">>, [{language, <<"pwr">>}, {name, <<"Powari"/utf8>>}, {name_en, <<"Powari"/utf8>>}]},
    {<<"guw">>, [{language, <<"guw">>}, {name, <<"Gun"/utf8>>}, {name_en, <<"Gun"/utf8>>}]},
    {<<"iso">>, [{language, <<"iso">>}, {name, <<"Isoko"/utf8>>}, {name_en, <<"Isoko"/utf8>>}]},
    {<<"mjw">>, [{language, <<"mjw">>}, {name, <<"Karbi"/utf8>>}, {name_en, <<"Karbi"/utf8>>}]},
    {<<"pem">>, [{language, <<"pem">>}, {name, <<"Phende"/utf8>>}, {name_en, <<"Phende"/utf8>>}]},
    {<<"zgn">>, [{language, <<"zgn">>}, {name, <<"Guibian Zhuang"/utf8>>}, {name_en, <<"Guibian Zhuang"/utf8>>}]},
    {<<"dyo">>, [{language, <<"dyo">>}, {name, <<"Jola-Fonyi"/utf8>>}, {name_en, <<"Jola-Fonyi"/utf8>>}]},
    {<<"gwr">>, [{language, <<"gwr">>}, {name, <<"Gwere"/utf8>>}, {name_en, <<"Gwere"/utf8>>}]},
    {<<"lag">>, [{language, <<"lag">>}, {name, <<"Langi"/utf8>>}, {name_en, <<"Langi"/utf8>>}]},
    {<<"mls">>, [{language, <<"mls">>}, {name, <<"Masalit"/utf8>>}, {name_en, <<"Masalit"/utf8>>}]},
    {<<"sml">>, [{language, <<"sml">>}, {name, <<"Central Sama"/utf8>>}, {name_en, <<"Central Sama"/utf8>>}]},
    {<<"ssb">>, [{language, <<"ssb">>}, {name, <<"Southern Sama"/utf8>>}, {name_en, <<"Southern Sama"/utf8>>}]},
    {<<"sse">>, [{language, <<"sse">>}, {name, <<"Balangingi"/utf8>>}, {name_en, <<"Balangingi"/utf8>>}]},
    {<<"lia">>, [{language, <<"lia">>}, {name, <<"West-Central Limba"/utf8>>}, {name_en, <<"West-Central Limba"/utf8>>}]},
    {<<"vmr">>, [{language, <<"vmr">>}, {name, <<"Marenje"/utf8>>}, {name_en, <<"Marenje"/utf8>>}]},
    {<<"efi">>, [{language, <<"efi">>}, {name, <<"Efik"/utf8>>}, {name_en, <<"Efik"/utf8>>}]},
    {<<"bfs">>, [{language, <<"bfs">>}, {name, <<"Southern Bai"/utf8>>}, {name_en, <<"Southern Bai"/utf8>>}]},
    {<<"dav">>, [{language, <<"dav">>}, {name, <<"Taita"/utf8>>}, {name_en, <<"Taita"/utf8>>}]},
    {<<"dig">>, [{language, <<"dig">>}, {name, <<"Digo"/utf8>>}, {name_en, <<"Digo"/utf8>>}]},
    {<<"flr">>, [{language, <<"flr">>}, {name, <<"Fuliiru"/utf8>>}, {name_en, <<"Fuliiru"/utf8>>}]},
    {<<"fmu">>, [{language, <<"fmu">>}, {name, <<"Far Western Muria"/utf8>>}, {name_en, <<"Far Western Muria"/utf8>>}]},
    {<<"hif">>, [{language, <<"hif">>}, {name, <<"Fiji Hindi"/utf8>>}, {name_en, <<"Fiji Hindi"/utf8>>}]},
    {<<"kmw">>, [{language, <<"kmw">>}, {name, <<"Komo (Democratic Republic of Congo)"/utf8>>}, {name_en, <<"Komo (Democratic Republic of Congo)"/utf8>>}]},
    {<<"lea">>, [{language, <<"lea">>}, {name, <<"Lega-Shabunda"/utf8>>}, {name_en, <<"Lega-Shabunda"/utf8>>}]},
    {<<"lol">>, [{language, <<"lol">>}, {name, <<"Mongo"/utf8>>}, {name_en, <<"Mongo"/utf8>>}]},
    {<<"lun">>, [{language, <<"lun">>}, {name, <<"Lunda"/utf8>>}, {name_en, <<"Lunda"/utf8>>}]},
    {<<"mji">>, [{language, <<"mji">>}, {name, <<"Kim Mun"/utf8>>}, {name_en, <<"Kim Mun"/utf8>>}]},
    {<<"mut">>, [{language, <<"mut">>}, {name, <<"Western Muria"/utf8>>}, {name_en, <<"Western Muria"/utf8>>}]},
    {<<"mxg">>, [{language, <<"mxg">>}, {name, <<"Mbangala"/utf8>>}, {name_en, <<"Mbangala"/utf8>>}]},
    {<<"mzm">>, [{language, <<"mzm">>}, {name, <<"Mumuye"/utf8>>}, {name_en, <<"Mumuye"/utf8>>}]},
    {<<"now">>, [{language, <<"now">>}, {name, <<"Nyambo"/utf8>>}, {name_en, <<"Nyambo"/utf8>>}]},
    {<<"sgd">>, [{language, <<"sgd">>}, {name, <<"Surigaonon"/utf8>>}, {name_en, <<"Surigaonon"/utf8>>}]},
    {<<"srx">>, [{language, <<"srx">>}, {name, <<"Sirmauri"/utf8>>}, {name_en, <<"Sirmauri"/utf8>>}]},
    {<<"tzo">>, [{language, <<"tzo">>}, {name, <<"Tzotzil"/utf8>>}, {name_en, <<"Tzotzil"/utf8>>}]},
    {<<"xhd">>, [{language, <<"xhd">>}, {name, <<"Hadrami"/utf8>>}, {name_en, <<"Hadrami"/utf8>>}]},
    {<<"dug">>, [{language, <<"dug">>}, {name, <<"Duruma"/utf8>>}, {name_en, <<"Duruma"/utf8>>}]},
    {<<"ltz">>, [{language, <<"ltz">>}, {name, <<"Luxembourgish"/utf8>>}, {name_en, <<"Luxembourgish"/utf8>>}]},
    {<<"grb">>, [{language, <<"grb">>}, {name, <<"Grebo"/utf8>>}, {name_en, <<"Grebo"/utf8>>}]},
    {<<"gry">>, [{language, <<"gry">>}, {name, <<"Barclayville Grebo"/utf8>>}, {name_en, <<"Barclayville Grebo"/utf8>>}]},
    {<<"mdf">>, [{language, <<"mdf">>}, {name, <<"Moksha"/utf8>>}, {name_en, <<"Moksha"/utf8>>}]},
    {<<"mev">>, [{language, <<"mev">>}, {name, <<"Mano"/utf8>>}, {name_en, <<"Mano"/utf8>>}]},
    {<<"myv">>, [{language, <<"myv">>}, {name, <<"Erzya"/utf8>>}, {name_en, <<"Erzya"/utf8>>}]},
    {<<"tdt">>, [{language, <<"tdt">>}, {name, <<"Tetun Dili"/utf8>>}, {name_en, <<"Tetun Dili"/utf8>>}]},
    {<<"dzg">>, [{language, <<"dzg">>}, {name, <<"Dazaga"/utf8>>}, {name_en, <<"Dazaga"/utf8>>}]},
    {<<"gbl">>, [{language, <<"gbl">>}, {name, <<"Gamit"/utf8>>}, {name_en, <<"Gamit"/utf8>>}]},
    {<<"krj">>, [{language, <<"krj">>}, {name, <<"Kinaray-A"/utf8>>}, {name_en, <<"Kinaray-A"/utf8>>}]},
    {<<"lif">>, [{language, <<"lif">>}, {name, <<"Limbu"/utf8>>}, {name_en, <<"Limbu"/utf8>>}]},
    {<<"mke">>, [{language, <<"mke">>}, {name, <<"Mawchi"/utf8>>}, {name_en, <<"Mawchi"/utf8>>}]},
    {<<"ziw">>, [{language, <<"ziw">>}, {name, <<"Zigula"/utf8>>}, {name_en, <<"Zigula"/utf8>>}]},
    {<<"ngb">>, [{language, <<"ngb">>}, {name, <<"Northern Ngbandi"/utf8>>}, {name_en, <<"Northern Ngbandi"/utf8>>}]},
    {<<"rah">>, [{language, <<"rah">>}, {name, <<"Rabha"/utf8>>}, {name_en, <<"Rabha"/utf8>>}]},
    {<<"wti">>, [{language, <<"wti">>}, {name, <<"Berta"/utf8>>}, {name_en, <<"Berta"/utf8>>}]},
    {<<"zlq">>, [{language, <<"zlq">>}, {name, <<"Liuqian Zhuang"/utf8>>}, {name_en, <<"Liuqian Zhuang"/utf8>>}]},
    {<<"gvr">>, [{language, <<"gvr">>}, {name, <<"Gurung"/utf8>>}, {name_en, <<"Gurung"/utf8>>}]},
    {<<"kff">>, [{language, <<"kff">>}, {name, <<"Koya"/utf8>>}, {name_en, <<"Koya"/utf8>>}]},
    {<<"kyk">>, [{language, <<"kyk">>}, {name, <<"Kamayo"/utf8>>}, {name_en, <<"Kamayo"/utf8>>}]},
    {<<"zmz">>, [{language, <<"zmz">>}, {name, <<"Mbandja"/utf8>>}, {name_en, <<"Mbandja"/utf8>>}]},
    {<<"gof">>, [{language, <<"gof">>}, {name, <<"Gofa"/utf8>>}, {name_en, <<"Gofa"/utf8>>}]},
    {<<"twx">>, [{language, <<"twx">>}, {name, <<"Tewe"/utf8>>}, {name_en, <<"Tewe"/utf8>>}]},
    {<<"gby">>, [{language, <<"gby">>}, {name, <<"Gbari"/utf8>>}, {name_en, <<"Gbari"/utf8>>}]},
    {<<"hig">>, [{language, <<"hig">>}, {name, <<"Kamwe"/utf8>>}, {name_en, <<"Kamwe"/utf8>>}]},
    {<<"hmq">>, [{language, <<"hmq">>}, {name, <<"Eastern Qiandong Miao"/utf8>>}, {name_en, <<"Eastern Qiandong Miao"/utf8>>}]},
    {<<"leb">>, [{language, <<"leb">>}, {name, <<"Lala-Bisa"/utf8>>}, {name_en, <<"Lala-Bisa"/utf8>>}]},
    {<<"lew">>, [{language, <<"lew">>}, {name, <<"Ledo Kaili"/utf8>>}, {name_en, <<"Ledo Kaili"/utf8>>}]},
    {<<"msb">>, [{language, <<"msb">>}, {name, <<"Masbatenyo"/utf8>>}, {name_en, <<"Masbatenyo"/utf8>>}]},
    {<<"nxq">>, [{language, <<"nxq">>}, {name, <<"Naxi"/utf8>>}, {name_en, <<"Naxi"/utf8>>}]},
    {<<"pdc">>, [{language, <<"pdc">>}, {name, <<"Pennsylvania German"/utf8>>}, {name_en, <<"Pennsylvania German"/utf8>>}]},
    {<<"peg">>, [{language, <<"peg">>}, {name, <<"Pengo"/utf8>>}, {name_en, <<"Pengo"/utf8>>}]},
    {<<"prd">>, [{language, <<"prd">>}, {name, <<"Parsi-Dari"/utf8>>}, {name_en, <<"Parsi-Dari"/utf8>>}]},
    {<<"spp">>, [{language, <<"spp">>}, {name, <<"Supyire Senoufo"/utf8>>}, {name_en, <<"Supyire Senoufo"/utf8>>}]},
    {<<"kxd">>, [{language, <<"kxd">>}, {name, <<"Brunei"/utf8>>}, {name_en, <<"Brunei"/utf8>>}]},
    {<<"div">>, [{language, <<"div">>}, {name, <<"Dhivehi"/utf8>>}, {name_en, <<"Dhivehi"/utf8>>}]},
    {<<"mfb">>, [{language, <<"mfb">>}, {name, <<"Bangka"/utf8>>}, {name_en, <<"Bangka"/utf8>>}]},
    {<<"nuj">>, [{language, <<"nuj">>}, {name, <<"Nyole"/utf8>>}, {name_en, <<"Nyole"/utf8>>}]},
    {<<"twd">>, [{language, <<"twd">>}, {name, <<"Twents"/utf8>>}, {name_en, <<"Twents"/utf8>>}]},
    {<<"fij">>, [{language, <<"fij">>}, {name, <<"Fijian"/utf8>>}, {name_en, <<"Fijian"/utf8>>}]},
    {<<"thq">>, [{language, <<"thq">>}, {name, <<"Kochila Tharu"/utf8>>}, {name_en, <<"Kochila Tharu"/utf8>>}]},
    {<<"rbl">>, [{language, <<"rbl">>}, {name, <<"Miraya Bikol"/utf8>>}, {name_en, <<"Miraya Bikol"/utf8>>}]},
    {<<"gej">>, [{language, <<"gej">>}, {name, <<"Gen"/utf8>>}, {name_en, <<"Gen"/utf8>>}]},
    {<<"knx">>, [{language, <<"knx">>}, {name, <<"Kendayan"/utf8>>}, {name_en, <<"Kendayan"/utf8>>}]},
    {<<"lbw">>, [{language, <<"lbw">>}, {name, <<"Tolaki"/utf8>>}, {name_en, <<"Tolaki"/utf8>>}]},
    {<<"nzi">>, [{language, <<"nzi">>}, {name, <<"Nzima"/utf8>>}, {name_en, <<"Nzima"/utf8>>}]},
    {<<"fue">>, [{language, <<"fue">>}, {name, <<"Borgu Fulfulde"/utf8>>}, {name_en, <<"Borgu Fulfulde"/utf8>>}]},
    {<<"prp">>, [{language, <<"prp">>}, {name, <<"Parsi"/utf8>>}, {name_en, <<"Parsi"/utf8>>}]},
    {<<"rup">>, [{language, <<"rup">>}, {name, <<"Macedo-Romanian"/utf8>>}, {name_en, <<"Macedo-Romanian"/utf8>>}]},
    {<<"knk">>, [{language, <<"knk">>}, {name, <<"Kuranko"/utf8>>}, {name_en, <<"Kuranko"/utf8>>}]},
    {<<"tdg">>, [{language, <<"tdg">>}, {name, <<"Western Tamang"/utf8>>}, {name_en, <<"Western Tamang"/utf8>>}]},
    {<<"dip">>, [{language, <<"dip">>}, {name, <<"Northeastern Dinka"/utf8>>}, {name_en, <<"Northeastern Dinka"/utf8>>}]},
    {<<"ebu">>, [{language, <<"ebu">>}, {name, <<"Embu"/utf8>>}, {name_en, <<"Embu"/utf8>>}]},
    {<<"grg">>, [{language, <<"grg">>}, {name, <<"Madi"/utf8>>}, {name_en, <<"Madi"/utf8>>}]},
    {<<"guc">>, [{language, <<"guc">>}, {name, <<"Wayuu"/utf8>>}, {name_en, <<"Wayuu"/utf8>>}]},
    {<<"inh">>, [{language, <<"inh">>}, {name, <<"Ingush"/utf8>>}, {name_en, <<"Ingush"/utf8>>}]},
    {<<"mhi">>, [{language, <<"mhi">>}, {name, <<"Ma'di"/utf8>>}, {name_en, <<"Ma'di"/utf8>>}]},
    {<<"rmo">>, [{language, <<"rmo">>}, {name, <<"Sinte Romani"/utf8>>}, {name_en, <<"Sinte Romani"/utf8>>}]},
    {<<"vay">>, [{language, <<"vay">>}, {name, <<"Wayu"/utf8>>}, {name_en, <<"Wayu"/utf8>>}]},
    {<<"qxo">>, [{language, <<"qxo">>}, {name, <<"Southern Conchucos Ancash Quechua"/utf8>>}, {name_en, <<"Southern Conchucos Ancash Quechua"/utf8>>}]},
    {<<"eyo">>, [{language, <<"eyo">>}, {name, <<"Keiyo"/utf8>>}, {name_en, <<"Keiyo"/utf8>>}]},
    {<<"zdj">>, [{language, <<"zdj">>}, {name, <<"Ngazidja Comorian"/utf8>>}, {name_en, <<"Ngazidja Comorian"/utf8>>}]},
    {<<"hbb">>, [{language, <<"hbb">>}, {name, <<"Huba"/utf8>>}, {name_en, <<"Huba"/utf8>>}]},
    {<<"krc">>, [{language, <<"krc">>}, {name, <<"Karachay-Balkar"/utf8>>}, {name_en, <<"Karachay-Balkar"/utf8>>}]},
    {<<"ljl">>, [{language, <<"ljl">>}, {name, <<"Li'o"/utf8>>}, {name_en, <<"Li'o"/utf8>>}]},
    {<<"mfv">>, [{language, <<"mfv">>}, {name, <<"Mandjak"/utf8>>}, {name_en, <<"Mandjak"/utf8>>}]},
    {<<"nxe">>, [{language, <<"nxe">>}, {name, <<"Nage"/utf8>>}, {name_en, <<"Nage"/utf8>>}]},
    {<<"nyd">>, [{language, <<"nyd">>}, {name, <<"Nyore"/utf8>>}, {name_en, <<"Nyore"/utf8>>}]},
    {<<"tkq">>, [{language, <<"tkq">>}, {name, <<"Tee"/utf8>>}, {name_en, <<"Tee"/utf8>>}]},
    {<<"lwg">>, [{language, <<"lwg">>}, {name, <<"Wanga"/utf8>>}, {name_en, <<"Wanga"/utf8>>}]},
    {<<"mrd">>, [{language, <<"mrd">>}, {name, <<"Western Magar"/utf8>>}, {name_en, <<"Western Magar"/utf8>>}]},
    {<<"fur">>, [{language, <<"fur">>}, {name, <<"Friulian"/utf8>>}, {name_en, <<"Friulian"/utf8>>}]},
    {<<"hmd">>, [{language, <<"hmd">>}, {name, <<"Large Flowery Miao"/utf8>>}, {name_en, <<"Large Flowery Miao"/utf8>>}]},
    {<<"ish">>, [{language, <<"ish">>}, {name, <<"Esan"/utf8>>}, {name_en, <<"Esan"/utf8>>}]},
    {<<"jab">>, [{language, <<"jab">>}, {name, <<"Hyam"/utf8>>}, {name_en, <<"Hyam"/utf8>>}]},
    {<<"jmc">>, [{language, <<"jmc">>}, {name, <<"Machame"/utf8>>}, {name_en, <<"Machame"/utf8>>}]},
    {<<"kad">>, [{language, <<"kad">>}, {name, <<"Adara"/utf8>>}, {name_en, <<"Adara"/utf8>>}]},
    {<<"kaj">>, [{language, <<"kaj">>}, {name, <<"Jju"/utf8>>}, {name_en, <<"Jju"/utf8>>}]},
    {<<"luc">>, [{language, <<"luc">>}, {name, <<"Aringa"/utf8>>}, {name_en, <<"Aringa"/utf8>>}]},
    {<<"mde">>, [{language, <<"mde">>}, {name, <<"Maba (Chad)"/utf8>>}, {name_en, <<"Maba (Chad)"/utf8>>}]},
    {<<"mnb">>, [{language, <<"mnb">>}, {name, <<"Muna"/utf8>>}, {name_en, <<"Muna"/utf8>>}]},
    {<<"mqa">>, [{language, <<"mqa">>}, {name, <<"Maba (Indonesia)"/utf8>>}, {name_en, <<"Maba (Indonesia)"/utf8>>}]},
    {<<"mqg">>, [{language, <<"mqg">>}, {name, <<"Kota Bangun Kutai Malay"/utf8>>}, {name_en, <<"Kota Bangun Kutai Malay"/utf8>>}]},
    {<<"mxx">>, [{language, <<"mxx">>}, {name, <<"Mahou"/utf8>>}, {name_en, <<"Mahou"/utf8>>}]},
    {<<"naq">>, [{language, <<"naq">>}, {name, <<"Khoekhoe"/utf8>>}, {name_en, <<"Khoekhoe"/utf8>>}]},
    {<<"nsd">>, [{language, <<"nsd">>}, {name, <<"Southern Nisu"/utf8>>}, {name_en, <<"Southern Nisu"/utf8>>}]},
    {<<"osi">>, [{language, <<"osi">>}, {name, <<"Osing"/utf8>>}, {name_en, <<"Osing"/utf8>>}]},
    {<<"phu">>, [{language, <<"phu">>}, {name, <<"Phuan"/utf8>>}, {name_en, <<"Phuan"/utf8>>}]},
    {<<"pil">>, [{language, <<"pil">>}, {name, <<"Yom"/utf8>>}, {name_en, <<"Yom"/utf8>>}]},
    {<<"say">>, [{language, <<"say">>}, {name, <<"Saya"/utf8>>}, {name_en, <<"Saya"/utf8>>}]},
    {<<"smw">>, [{language, <<"smw">>}, {name, <<"Sumbawa"/utf8>>}, {name_en, <<"Sumbawa"/utf8>>}]},
    {<<"sur">>, [{language, <<"sur">>}, {name, <<"Mwaghavul"/utf8>>}, {name_en, <<"Mwaghavul"/utf8>>}]},
    {<<"swi">>, [{language, <<"swi">>}, {name, <<"Sui"/utf8>>}, {name_en, <<"Sui"/utf8>>}]},
    {<<"tec">>, [{language, <<"tec">>}, {name, <<"Terik"/utf8>>}, {name_en, <<"Terik"/utf8>>}]},
    {<<"tgh">>, [{language, <<"tgh">>}, {name, <<"Tobagonian Creole English"/utf8>>}, {name_en, <<"Tobagonian Creole English"/utf8>>}]},
    {<<"vun">>, [{language, <<"vun">>}, {name, <<"Vunjo"/utf8>>}, {name_en, <<"Vunjo"/utf8>>}]},
    {<<"wsg">>, [{language, <<"wsg">>}, {name, <<"Adilabad Gondi"/utf8>>}, {name_en, <<"Adilabad Gondi"/utf8>>}]},
    {<<"wxa">>, [{language, <<"wxa">>}, {name, <<"Waxianghua"/utf8>>}, {name_en, <<"Waxianghua"/utf8>>}]},
    {<<"xmc">>, [{language, <<"xmc">>}, {name, <<"Makhuwa-Marrevone"/utf8>>}, {name_en, <<"Makhuwa-Marrevone"/utf8>>}]},
    {<<"ybb">>, [{language, <<"ybb">>}, {name, <<"Yemba"/utf8>>}, {name_en, <<"Yemba"/utf8>>}]},
    {<<"yer">>, [{language, <<"yer">>}, {name, <<"Tarok"/utf8>>}, {name_en, <<"Tarok"/utf8>>}]},
    {<<"qxn">>, [{language, <<"qxn">>}, {name, <<"Northern Conchucos Ancash Quechua"/utf8>>}, {name_en, <<"Northern Conchucos Ancash Quechua"/utf8>>}]},
    {<<"kfs">>, [{language, <<"kfs">>}, {name, <<"Bilaspuri"/utf8>>}, {name_en, <<"Bilaspuri"/utf8>>}]},
    {<<"kfu">>, [{language, <<"kfu">>}, {name, <<"Katkari"/utf8>>}, {name_en, <<"Katkari"/utf8>>}]},
    {<<"gxx">>, [{language, <<"gxx">>}, {name, <<"Wè Southern"/utf8>>}, {name_en, <<"Wè Southern"/utf8>>}]},
    {<<"bft">>, [{language, <<"bft">>}, {name, <<"Balti"/utf8>>}, {name_en, <<"Balti"/utf8>>}]},
    {<<"kdh">>, [{language, <<"kdh">>}, {name, <<"Tem"/utf8>>}, {name_en, <<"Tem"/utf8>>}]},
    {<<"kvr">>, [{language, <<"kvr">>}, {name, <<"Kerinci"/utf8>>}, {name_en, <<"Kerinci"/utf8>>}]},
    {<<"kqs">>, [{language, <<"kqs">>}, {name, <<"Northern Kissi"/utf8>>}, {name_en, <<"Northern Kissi"/utf8>>}]},
    {<<"the">>, [{language, <<"the">>}, {name, <<"Chitwania Tharu"/utf8>>}, {name_en, <<"Chitwania Tharu"/utf8>>}]},
    {<<"ior">>, [{language, <<"ior">>}, {name, <<"Inor"/utf8>>}, {name_en, <<"Inor"/utf8>>}]},
    {<<"kby">>, [{language, <<"kby">>}, {name, <<"Manga Kanuri"/utf8>>}, {name_en, <<"Manga Kanuri"/utf8>>}]},
    {<<"kzn">>, [{language, <<"kzn">>}, {name, <<"Kokola"/utf8>>}, {name_en, <<"Kokola"/utf8>>}]},
    {<<"mne">>, [{language, <<"mne">>}, {name, <<"Naba"/utf8>>}, {name_en, <<"Naba"/utf8>>}]},
    {<<"taq">>, [{language, <<"taq">>}, {name, <<"Tamasheq"/utf8>>}, {name_en, <<"Tamasheq"/utf8>>}]},
    {<<"tmh">>, [{language, <<"tmh">>}, {name, <<"Tamashek"/utf8>>}, {name_en, <<"Tamashek"/utf8>>}]},
    {<<"tyv">>, [{language, <<"tyv">>}, {name, <<"Tuvinian"/utf8>>}, {name_en, <<"Tuvinian"/utf8>>}]},
    {<<"ets">>, [{language, <<"ets">>}, {name, <<"Yekhee"/utf8>>}, {name_en, <<"Yekhee"/utf8>>}]},
    {<<"wss">>, [{language, <<"wss">>}, {name, <<"Wasa"/utf8>>}, {name_en, <<"Wasa"/utf8>>}]},
    {<<"pap">>, [{language, <<"pap">>}, {name, <<"Papiamento"/utf8>>}, {name_en, <<"Papiamento"/utf8>>}]},
    {<<"her">>, [{language, <<"her">>}, {name, <<"Herero"/utf8>>}, {name_en, <<"Herero"/utf8>>}]},
    {<<"snl">>, [{language, <<"snl">>}, {name, <<"Sangil"/utf8>>}, {name_en, <<"Sangil"/utf8>>}]},
    {<<"srv">>, [{language, <<"srv">>}, {name, <<"Southern Sorsoganon"/utf8>>}, {name_en, <<"Southern Sorsoganon"/utf8>>}]},
    {<<"tcz">>, [{language, <<"tcz">>}, {name, <<"Thado Chin"/utf8>>}, {name_en, <<"Thado Chin"/utf8>>}]},
    {<<"zag">>, [{language, <<"zag">>}, {name, <<"Zaghawa"/utf8>>}, {name_en, <<"Zaghawa"/utf8>>}]},
    {<<"gya">>, [{language, <<"gya">>}, {name, <<"Northwest Gbaya"/utf8>>}, {name_en, <<"Northwest Gbaya"/utf8>>}]},
    {<<"wni">>, [{language, <<"wni">>}, {name, <<"Ndzwani Comorian"/utf8>>}, {name_en, <<"Ndzwani Comorian"/utf8>>}]},
    {<<"iqw">>, [{language, <<"iqw">>}, {name, <<"Ikwo"/utf8>>}, {name_en, <<"Ikwo"/utf8>>}]},
    {<<"jra">>, [{language, <<"jra">>}, {name, <<"Jarai"/utf8>>}, {name_en, <<"Jarai"/utf8>>}]},
    {<<"las">>, [{language, <<"las">>}, {name, <<"Lama (Togo)"/utf8>>}, {name_en, <<"Lama (Togo)"/utf8>>}]},
    {<<"mck">>, [{language, <<"mck">>}, {name, <<"Mbunda"/utf8>>}, {name_en, <<"Mbunda"/utf8>>}]},
    {<<"mse">>, [{language, <<"mse">>}, {name, <<"Musey"/utf8>>}, {name_en, <<"Musey"/utf8>>}]},
    {<<"nhq">>, [{language, <<"nhq">>}, {name, <<"Huaxcaleca Nahuatl"/utf8>>}, {name_en, <<"Huaxcaleca Nahuatl"/utf8>>}]},
    {<<"nhy">>, [{language, <<"nhy">>}, {name, <<"Northern Oaxaca Nahuatl"/utf8>>}, {name_en, <<"Northern Oaxaca Nahuatl"/utf8>>}]},
    {<<"nhz">>, [{language, <<"nhz">>}, {name, <<"Santa María La Alta Nahuatl"/utf8>>}, {name_en, <<"Santa María La Alta Nahuatl"/utf8>>}]},
    {<<"njo">>, [{language, <<"njo">>}, {name, <<"Ao Naga"/utf8>>}, {name_en, <<"Ao Naga"/utf8>>}]},
    {<<"nkn">>, [{language, <<"nkn">>}, {name, <<"Nkangala"/utf8>>}, {name_en, <<"Nkangala"/utf8>>}]},
    {<<"npl">>, [{language, <<"npl">>}, {name, <<"Southeastern Puebla Nahuatl"/utf8>>}, {name_en, <<"Southeastern Puebla Nahuatl"/utf8>>}]},
    {<<"nsu">>, [{language, <<"nsu">>}, {name, <<"Sierra Negra Nahuatl"/utf8>>}, {name_en, <<"Sierra Negra Nahuatl"/utf8>>}]},
    {<<"pce">>, [{language, <<"pce">>}, {name, <<"Ruching Palaung"/utf8>>}, {name_en, <<"Ruching Palaung"/utf8>>}]},
    {<<"gru">>, [{language, <<"gru">>}, {name, <<"Kistane"/utf8>>}, {name_en, <<"Kistane"/utf8>>}]},
    {<<"sxn">>, [{language, <<"sxn">>}, {name, <<"Sangir"/utf8>>}, {name_en, <<"Sangir"/utf8>>}]},
    {<<"lkb">>, [{language, <<"lkb">>}, {name, <<"Kabras"/utf8>>}, {name_en, <<"Kabras"/utf8>>}]},
    {<<"dub">>, [{language, <<"dub">>}, {name, <<"Dubli"/utf8>>}, {name_en, <<"Dubli"/utf8>>}]},
    {<<"hmz">>, [{language, <<"hmz">>}, {name, <<"Hmong Shua"/utf8>>}, {name_en, <<"Hmong Shua"/utf8>>}]},
    {<<"vkt">>, [{language, <<"vkt">>}, {name, <<"Tenggarong Kutai Malay"/utf8>>}, {name_en, <<"Tenggarong Kutai Malay"/utf8>>}]},
    {<<"dks">>, [{language, <<"dks">>}, {name, <<"Southeastern Dinka"/utf8>>}, {name_en, <<"Southeastern Dinka"/utf8>>}]},
    {<<"eto">>, [{language, <<"eto">>}, {name, <<"Eton (Cameroon)"/utf8>>}, {name_en, <<"Eton (Cameroon)"/utf8>>}]},
    {<<"hdy">>, [{language, <<"hdy">>}, {name, <<"Hadiyya"/utf8>>}, {name_en, <<"Hadiyya"/utf8>>}]},
    {<<"ige">>, [{language, <<"ige">>}, {name, <<"Igede"/utf8>>}, {name_en, <<"Igede"/utf8>>}]},
    {<<"ilm">>, [{language, <<"ilm">>}, {name, <<"Iranun (Malaysia)"/utf8>>}, {name_en, <<"Iranun (Malaysia)"/utf8>>}]},
    {<<"ilp">>, [{language, <<"ilp">>}, {name, <<"Iranun (Philippines)"/utf8>>}, {name_en, <<"Iranun (Philippines)"/utf8>>}]},
    {<<"ksh">>, [{language, <<"ksh">>}, {name, <<"Kölsch"/utf8>>}, {name_en, <<"Kölsch"/utf8>>}]},
    {<<"kvx">>, [{language, <<"kvx">>}, {name, <<"Parkari Koli"/utf8>>}, {name_en, <<"Parkari Koli"/utf8>>}]},
    {<<"lpo">>, [{language, <<"lpo">>}, {name, <<"Lipo"/utf8>>}, {name_en, <<"Lipo"/utf8>>}]},
    {<<"mry">>, [{language, <<"mry">>}, {name, <<"Mandaya"/utf8>>}, {name_en, <<"Mandaya"/utf8>>}]},
    {<<"nbe">>, [{language, <<"nbe">>}, {name, <<"Konyak Naga"/utf8>>}, {name_en, <<"Konyak Naga"/utf8>>}]},
    {<<"nnh">>, [{language, <<"nnh">>}, {name, <<"Ngiemboon"/utf8>>}, {name_en, <<"Ngiemboon"/utf8>>}]},
    {<<"nos">>, [{language, <<"nos">>}, {name, <<"Eastern Nisu"/utf8>>}, {name_en, <<"Eastern Nisu"/utf8>>}]},
    {<<"pbv">>, [{language, <<"pbv">>}, {name, <<"Pnar"/utf8>>}, {name_en, <<"Pnar"/utf8>>}]},
    {<<"qvw">>, [{language, <<"qvw">>}, {name, <<"Huaylla Wanca Quechua"/utf8>>}, {name_en, <<"Huaylla Wanca Quechua"/utf8>>}]},
    {<<"rnd">>, [{language, <<"rnd">>}, {name, <<"Ruund"/utf8>>}, {name_en, <<"Ruund"/utf8>>}]},
    {<<"sfw">>, [{language, <<"sfw">>}, {name, <<"Sehwi"/utf8>>}, {name_en, <<"Sehwi"/utf8>>}]},
    {<<"srb">>, [{language, <<"srb">>}, {name, <<"Sora"/utf8>>}, {name_en, <<"Sora"/utf8>>}]},
    {<<"tap">>, [{language, <<"tap">>}, {name, <<"Taabwa"/utf8>>}, {name_en, <<"Taabwa"/utf8>>}]},
    {<<"thz">>, [{language, <<"thz">>}, {name, <<"Tayart Tamajeq"/utf8>>}, {name_en, <<"Tayart Tamajeq"/utf8>>}]},
    {<<"xsm">>, [{language, <<"xsm">>}, {name, <<"Kasem"/utf8>>}, {name_en, <<"Kasem"/utf8>>}]},
    {<<"ywq">>, [{language, <<"ywq">>}, {name, <<"Wuding-Luquan Yi"/utf8>>}, {name_en, <<"Wuding-Luquan Yi"/utf8>>}]}
].

%% Other languages, used by less than 250000 speakers according to
%% https://www.languagecourse.net/languages-worldwide and excluding those with
%% only 3-letter language codes (for brevity):

% ae: Avestan
% ak: Akan
% an: Aragonese
% bm: Bambara
% cr: Cree
% dv: Dhivehi
% ee: Ewe
% ff: Fulah
% gv: Manx
% ha: Hausa
% ho: Hiri Motu
% ht: Haitian
% hy: Armenian
% hz: Herero
% ie: Interlingue
% ig: Igbo
% ii: Sichuan Yi
% ik: Inupiak
% io: Ido
% iu: Inuktitut
% kg: Kongo
% ki: Kikuyu
% kj: Kuanyama
% kk: Kazakh
% kl: Kalaallisut Greenlandic
% km: Khmer Cambodian
% kn: Kannada
% kr: Kanuri
% ks: Kashmiri
% kv: Komi
% kw: Cornish
% ky: Kirghiz
% la: Latin
% lb: Luxembourgish/Letzeburgesch
% lg: Ganda/Luganda
% li: Limburgan/Limburger
% ln: Lingala
% lo: Lao Laotian
% lu: Luba-Katanga
% mh: Marshall
% mi: Maori
% ml: Malayalam
% mr: Marathi
% my: Burmese
% na: Nauru
% nb: Norwegian Bokmål
% nd: Ndebele, North
% ne: Nepali
% ng: Ndonga
% nr: Ndebele, South
% nv: Navajo
% ny: Chichewa Nyanja
% oc: Occitan Provençal
% oj: Ojibwa
% om: (Afan) Oromo
% or: Oriya
% os: Ossetian Ossetic
% pi: Pali
% qu: Quechua
% rm: Rhaeto-Romance
% rn: Rundi Kirundi
% rw: Kinyarwanda
% sa: Sanskrit
% sc: Sardinian
% sd: Sindhi
% se: Northern Sami
% sg: Sango Sangro
% sh: Serbo-Croatian
% sm: Samoan
% sn: Shona
% so: Somali
% ss: Swati Siswati
% st: Sesotho Sotho, Southern
% te: Telugu
% tg: Tajik
% ti: Tigrinya
% tk: Turkmen
% tn: Tswana Setswana
% to: Tonga
% ts: Tsonga
% tt: Tatar
% tw: Twi
% ty: Tahitian
% ug: Uighur
% uz: Uzbek
% ve: Venda
% vo: Volapuk
% wa: Walloon
% wo: Wolof
% yi: Yiddish
% za: Zhuang
% zu: Zulu

%% Collections, special and private use:

% aav: Austro-Asiatic languages
% afa: Afro-Asiatic languages
% alg: Algonquian languages
% alv: Atlantic-Congo languages
% apa: Apache languages
% aqa: Alacalufan languages
% aql: Algic languages
% art: Artificial languages
% ath: Athapascan languages
% auf: Arauan languages
% aus: Australian languages
% awd: Arawakan languages
% azc: Uto-Aztecan languages
% bad: Banda languages
% bai: Bamileke languages
% bat: Baltic languages
% ber: Berber languages
% bnt: Bantu languages
% btk: Batak languages
% cai: Central American Indian languages
% cau: Caucasian languages
% cba: Chibchan languages
% ccn: North Caucasian languages
% ccs: South Caucasian languages
% cdc: Chadic languages
% cdd: Caddoan languages
% cel: Celtic languages
% cmc: Chamic languages
% cpe: English-based creoles and pidgins
% cpf: French-based creoles and pidgins
% cpp: Portuguese-based creoles and pidgins
% crp: Creoles and pidgins
% csu: Central Sudanic languages
% cus: Cushitic languages
% day: Land Dayak languages
% dmn: Mande languages
% dra: Dravidian languages
% egx: Egyptian languages
% esx: Eskimo-Aleut languages
% euq: Basque (family)
% fiu: Finno-Ugrian languages
% fox: Formosan languages
% gem: Germanic languages
% gme: East Germanic languages
% gmq: North Germanic languages
% gmw: West Germanic languages
% grk: Greek languages
% him: Himachali languages: Western Pahari languages
% hmx: Hmong-Mien languages
% hok: Hokan languages
% hyx: Armenian (family)
% iir: Indo-Iranian languages
% ijo: Ijo languages
% inc: Indic languages
% ine: Indo-European languages
% ira: Iranian languages
% iro: Iroquoian languages
% itc: Italic languages
% jpx: Japanese (family)
% kar: Karen languages
% kdo: Kordofanian languages
% khi: Khoisan languages
% kro: Kru languages
% map: Austronesian languages
% mkh: Mon-Khmer languages
% mno: Manobo languages
% mun: Munda languages
% myn: Mayan languages
% nah: Nahuatl languages
% nai: North American Indian languages
% ngf: Trans-New Guinea languages
% nic: Niger-Kordofanian languages
% nub: Nubian languages
% omq: Oto-Manguean languages
% omv: Omotic languages
% oto: Otomian languages
% paa: Papuan languages
% phi: Philippine languages
% plf: Central Malayo-Polynesian languages
% poz: Malayo-Polynesian languages
% pqe: Eastern Malayo-Polynesian languages
% pqw: Western Malayo-Polynesian languages
% pra: Prakrit languages
% qwe: Quechuan (family)
% roa: Romance languages
% sai: South American Indian languages
% sal: Salishan languages
% sdv: Eastern Sudanic languages
% sem: Semitic languages
% sgn: Sign languages
% sio: Siouan languages
% sit: Sino-Tibetan languages
% sla: Slavic languages
% smi: Sami languages
% son: Songhai languages
% sqj: Albanian languages
% ssa: Nilo-Saharan languages
% syd: Samoyedic languages
% tai: Tai languages
% tbq: Tibeto-Burman languages
% trk: Turkic languages
% tup: Tupi languages
% tut: Altaic languages
% tuw: Tungus languages
% urj: Uralic languages
% wak: Wakashan languages
% wen: Sorbian languages
% xgn: Mongolian languages
% xnd: Na-Dene languages
% ypk: Yupik languages
% zhx: Chinese (family)
% zle: East Slavic languages
% zls: South Slavic languages
% zlw: West Slavic languages
% znd: Zande languages
% mis: Uncoded languages
% mul: Multiple languages
% und: Undetermined
% zxx: No linguistic content/Not applicable
% qaa..qtz: Private use

%% Deprecated:

% in: Indonesian
% iw: Hebrew
% ji: Yiddish
% jw: Javanese
% mo: Moldavian
% aam: Aramanik
% adp: Adap
% agp: Paranan
% ais: Nataoran Amis
% ajp: South Levantine Arabic
% ajt: Judeo-Tunisian Arabic
% aoh: Arma
% asd: Asas
% aue: ǂKxʼauǁʼein
% ayx: Ayi (China)
% ayy: Tayabas Ayta
% baz: Tunen
% bbz: Babalia Creole Arabic
% bgm: Baga Mboteni
% bhk: Albay Bicolano
% bic: Bikaru
% bij: Vaghat-Ya-Bijim-Legeri
% bjd: Bandjigali
% bjq: Southern Betsimisaraka Malagasy
% bkb: Finallig
% blg: Balau
% bmy: Bemba (Democratic Republic of Congo)
% bpb: Barbacoas
% btb: Beti (Cameroon)
% btl: Bhatola
% bxx: Borna (Democratic Republic of Congo)
% byy: Buya
% cbe: Chipiajes
% cbh: Cagua
% cca: Cauca
% ccq: Chaungtha
% cdg: Chamari
% cjr: Chorotega
% cka: Khumi Awa Chin
% cmk: Chimakum
% coy: Coyaima
% cqu: Chilean Quechua
% cug: Chungmboko
% cum: Cumeral
% daf: Dan
% dap: Nisi (India)
% dek: Dek
% dgu: Degaru
% dha: Dhanwar (India)
% dit: Dirari
% djl: Djiwarli
% dkl: Kolum So Dogon
% drh: Darkhat
% drr: Dororo
% drw: Darwazi
% dud: Hun-Saare
% duj: Dhuwal
% dwl: Walo Kumbe Dogon
% ekc: Eastern Karnic
% elp: Elpaputih
% emo: Emok
% gav: Gabutamon
% gbc: Garawa
% gfx: Mangetti Dune ǃXung
% ggn: Eastern Gurung
% ggo: Southern Gondi
% ggr: Aghu Tharnggalu
% gio: Gelao
% gji: Geji
% gli: Guliguli
% gti: Gbati-ri
% guv: Gey
% hrr: Horuru
% iap: Iapama
% ibi: Ibilo
% ill: Iranun
% ilw: Talur
% ime: Imeraguen
% izi: Izi-Ezaa-Ikwo-Mgbo
% jar: Jarawa (Nigeria)
% jeg: Jeng
% kbf: Kakauhua
% kdv: Kado
% kgc: Kasseng
% kgd: Kataang
% kgh: Upper Tanudan Kalinga
% kgm: Karipúna
% kjf: Khalaj [Indo-Iranian]
% koj: Sara Dunjo
% kox: Coxima
% kpp: Paku Karen
% krm: Krim
% ksa: Shuwa-Zamani
% ktr: Kota Marudu Tinagas
% kvs: Kunggara
% kwq: Kwak
% kxe: Kakihum
% kxl: Nepali Kurux
% kxu: Kui (India)
% kzh: Kenuzi-Dongola
% kzj: Coastal Kadazan
% kzt: Tambunan Dusun
% lak: Laka (Nigeria)
% lba: Lui
% leg: Lengua
% lii: Lingkhim
% llo: Khlor
% lmm: Lamam
% lmz: Lumbee
% lno: Lango (South Sudan)
% lsg: Lyons Sign Language
% meg: Mea
% mgx: Omati
% mhh: Maskoy Pidgin
% mja: Mahei
% mld: Malakhel
% mnt: Maykulan
% mof: Mohegan-Montauk-Narragansett
% mst: Cataelano Mandaya
% mvm: Muya
% mwd: Mudbura
% mwj: Maligo
% mwx: Mediak
% mwy: Mosiro
% myd: Maramba
% myi: Mina (India)
% myq: Forest Maninka
% myt: Sangab Mandaya
% nad: Nijadali
% nbf: Naxi
% nbx: Ngura
% ncp: Ndaktup
% ngo: Ngoni
% nln: Durango Nahuatl
% nlr: Ngarla
% nns: Ningye
% nnx: Ngong
% nom: Nocamán
% noo: Nootka
% nte: Nathembo
% nts: Natagaimas
% nxu: Narau
% ome: Omejes
% oun: ǃOǃung
% pat: Papitalai
% pbz: Palu
% pcr: Panang
% pgy: Pongyong
% pii: Pini
% plj: Polci
% plp: Palpa
% pmc: Palumata
% pmk: Pamlico
% pmu: Mirpur Panjabi
% pod: Ponares
% ppa: Pao
% ppr: Piru
% prb: Lua'
% prp: Parsi
% pry: Pray 3
% puk: Pu Ko
% puz: Purum Naga
% rie: Rien
% rmr: Caló
% rna: Runa
% rsi: Rennellese Sign Language
% sap: Sanapaná
% sca: Sansu
% sdm: Semandang
% sgl: Sanglechi-Ishkashimi
% sgo: Songa
% skk: Sok
% slq: Salchuq
% smd: Sama
% snb: Sebuyau
% snh: Shinabo
% sul: Surigaonon
% sum: Sumo-Mayangna
% svr: Savara
% szd: Seru
% tbb: Tapeba
% tdu: Tempasuk Dusun
% tgg: Tangga
% thc: Tai Hang Tong
% thw: Thudam
% thx: The
% tid: Tidong
% tie: Tingal
% tkk: Takpa
% tlw: South Wemale
% tmk: Northwestern Tamang
% tmp: Tai Mène
% tne: Tinoc Kallahan
% tnf: Tangshewi
% toe: Tomedes
% tpw: Tupí
% tsf: Southwestern Tamang
% unp: Worora
% uok: Uokha
% uun: Kulon-Pazeh
% vki: Ija-Zuba
% wgw: Wagawaga
% wit: Wintu
% wiw: Wirangu
% wra: Warapu
% wrd: Warduji
% wya: Wyandot
% xba: Kamba (Brazil)
% xbx: Kabixí
% xia: Xiandao
% xip: Xipináwa
% xkh: Karahawyana
% xrq: Karranga
% xss: Assan
% xtz: Tasmanian
% ybd: Yangbye
% yds: Yiddish Sign Language
% yen: Yendang
% yiy: Yir Yoront
% yma: Yamphe
% ymt: Mator-Taygi-Karagas
% ynh: Yangho
% yos: Yos
% yri: Yarí
% yuu: Yugh
% zir: Ziriya
% zkb: Koibal
% zua: Zeem
