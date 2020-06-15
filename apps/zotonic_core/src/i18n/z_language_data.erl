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
        fallback/1,
        codes_bin/0,
        codes_atom/0,

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
fetch_fallback([ {CodeBin, Props} | List ], Path, Acc) ->
    CodeAtom = z_convert:to_atom(CodeBin),
    PathR = lists:reverse(Path),
    Acc1 = Acc#{
        CodeBin => PathR,
        CodeAtom => PathR
    },
    Sub = proplists:get_value(sublanguages, Props, []),
    Acc2 = fetch_fallback(Sub, [ CodeAtom | Path ], Acc1),
    fetch_fallback(List, Path, Acc2).

as_map_flat() ->
    as_map_flat(languages_list(), [], #{}).

as_map_flat(List, Fallback, Map) ->
    lists:foldl(
        fun({Code, Props}, Acc) ->
            CodeAtom = binary_to_atom(Code, utf8),
            MapProps = as_map_flat_props(CodeAtom, Props, Fallback),
            Acc1 = Acc#{
                CodeAtom => MapProps,
                Code => MapProps
            },
            as_map_flat(proplists:get_value(sublanguages, Props, []), Fallback, Acc1)
        end,
        Map,
        List).

as_map_flat_props(CodeAtom, Props, Fallback) ->
    M = maps:from_list(Props),
    M#{
        language_atom => binary_to_atom( maps:get(language, M), utf8 ),
        sublanguages => as_atom_map(maps:get(sublanguages, M, []), [ CodeAtom | Fallback ], #{}),
        fallback => Fallback,
        sort_key => z_string:to_lower( proplists:get_value(name_en, Props) )
    }.

as_atom_map() ->
    as_atom_map(languages_list(), [], #{}).

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
        sublanguages => as_atom_map(maps:get(sublanguages, M, []), [ CodeAtom | Fallback ], #{}),
        fallback => Fallback,
        sort_key => z_string:to_lower( proplists:get_value(name_en, Props) )
    }.

-spec compile(atom(), list(), list(), map()) -> binary().
compile(Module, Ls, LsA, Fallback) ->
    {ok, Module, Bin} = compile:forms(forms(Module, Ls, LsA, Fallback),
                                      [verbose, report_errors]),
    Bin.

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
        [ erl_syntax:clause([], none, [ erl_syntax:abstract( as_map_flat() )]) ]),
    erl_syntax:function(
        erl_syntax:atom(languages_map_main),
        [ erl_syntax:clause([], none, [erl_syntax:abstract( as_atom_map() )]) ]),
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
        {name, <<"Polszczyzna"/utf8>>},
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
    {<<"sv">>, [
        {language, <<"sv">>},
        {name, <<"Svenska"/utf8>>},
        {name_en, <<"Swedish"/utf8>>}
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
    {<<"zh">>, [
        {type, <<"macro_language">>},
        {language, <<"zh">>},
        {script, <<"Hans">>},
        {name, <<"中文"/utf8>>},
        {name_en, <<"Chinese (Simplified)"/utf8>>},
        {sublanguages, [
            {<<"zh-hans">>, [
                {language, <<"zh">>},
                {script, <<"Hans">>},
                {name, <<"简体中文"/utf8>>},
                {name_en, <<"Chinese (Simplified)"/utf8>>}
            ]},
            {<<"zh-hans-cn">>, [
                {language, <<"zh-hans">>},
                {region, <<"CN">>},
                {script, <<"Hans">>},
                {name, <<"中国大陆简体脚本"/utf8>>},
                {name_en, <<"Chinese - Mainland (Simplified)"/utf8>>}
            ]},
            {<<"zh-hans-sg">>, [
                {language, <<"zh-hans">>},
                {region, <<"SG">>},
                {script, <<"Hans">>},
                {name, <<"新加坡中国简体脚本"/utf8>>},
                {name_en, <<"Chinese - Singapore (Simplified)"/utf8>>}
            ]}
        ]}
    ]},
    {<<"zh-hant">>, [
        {language, <<"zh-hant">>},
        {script, <<"Hant">>},
        {name, <<"中國傳統的腳本"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>},
        {sublanguages, [
            {<<"zh-hant-hk">>, [
                {language, <<"zh-hant">>},
                {region, <<"HK">>},
                {script, <<"Hant">>},
                {name, <<"香港中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Hong Kong (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-mo">>, [
                {language, <<"zh-hant">>},
                {region, <<"MO">>},
                {script, <<"Hant">>},
                {name, <<"澳門中國人在傳統的腳本"/utf8>>},
                {name_en, <<"Chinese - Macau (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-tw">>, [
                {language, <<"zh-hant">>},
                {region, <<"TW">>},
                {script, <<"Hant">>},
                {name, <<"台灣中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Taiwan (Traditional)"/utf8>>}
            ]}
        ]}
    ]}
].



%% Other, less used languages:

% gv: Manx
% ha: Hausa
% ho: Hiri Motu
% hy: Armenian
% hz: Herero
% ik: Inupiak
% io: Ido
% iu: Inuktitut
% ki: Kikuyu
% kj: Kuanyama
% kk: Kazakh
% kl: Kalaallisut Greenlandic
% km: Khmer Cambodian
% kn: Kannada
% ks: Kashmiri
% ku: Kurdish
% kv: Komi
% kw: Cornish
% ky: Kirghiz
% lb: Letzeburgesch
% ln: Lingala
% lo: Lao Laotian
% mh: Marshall
% mi: Maori
% ml: Malayalam
% mo: Moldavian
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
% sm: Samoan
% sn: Shona
% so: Somali
% ss: Swati Siswati
% st: Sesotho Sotho, Southern
% su: Sundanese
% sw: Swahili
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
% ur: Urdu
% uz: Uzbek
% vo: Volapuk
% wa: Walloon
% wo: Wolof
% yo: Yoruba
% za: Zhuang
% zu: Zulu


