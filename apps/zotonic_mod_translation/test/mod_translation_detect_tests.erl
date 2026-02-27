%% @hidden

-module(mod_translation_detect_tests).
-moduledoc("
EUnit tests for language detection tokenization and n-gram matching.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% test tokenize function
tokenize_test() ->
    Data = <<"crazy stuf in here ,+_ A \t 123 test">>,
    X = translation_detect:tokenize(Data),
    ?assertEqual(X, ["crazy", "stuf", "in" , "here", "a", "test"]).

ngram_count_test() ->
    P0 = dict:to_list(translation_detect:ngram_count(2, ["words"], dict:new())),
    ?assertEqual(P0, [{"or",1},
                      {"rd",1},
                      {"s_",1},
                      {"ds",1},
                      {"wo",1},
                      {"_w",1}]),
    P1 = dict:to_list(translation_detect:ngram_count(3, ["words"], dict:new())),
    ?assertEqual(P1, [{"ord",1},
                      {"s__",1},
                      {"rds",1},
                      {"wor",1},
                      {"_wo",1},
                      {"ds_",1}]),
    P2 = dict:to_list(translation_detect:ngram_count(4, ["words"], dict:new())),
    ?assertEqual(P2, [{"s___",1},
                      {"rds_",1},
                      {"ords",1},
                      {"ds__",1},
                      {"_wor",1},
                      {"word",1}]),
    P3 = dict:to_list(translation_detect:ngram_count(5, ["words"], dict:new())),
    ?assertEqual(P3, [{"s____",1},
                      {"rds__",1},
                      {"ords_",1},
                      {"_word",1},
                      {"words",1},
                      {"ds___",1}]),
    P4 = dict:to_list(translation_detect:ngram_count(6, ["words"], dict:new())),
    ?assertEqual(P4, []).

detect_en_es_test() ->
    {ok, Es} = translation_detect:detect(<<"para poner este importante proyecto en práctica"/utf8>>),
    ?assertEqual(es, Es),
    {ok, En} = translation_detect:detect(<<"this is a test of the Emergency text categorizing system."/utf8>>),
    ?assertEqual(en, En).

detect_fr_it_test() ->
    {ok, Fr} = translation_detect:detect(<<"serait désigné peu après PDG d'Antenne 2 et de FR 3. Pas même lui ! Le"/utf8>>),
    ?assertEqual(fr, Fr),
    {ok, It} = translation_detect:detect(<<"studio dell'uomo interiore? La scienza del cuore umano, che"/utf8>>),
    ?assertEqual(it, It).

detect_ro_pl_test() ->
    {ok, Ro} = translation_detect:detect(<<"taiate pe din doua, in care vezi stralucind brun  sau violet cristalele interioare"/utf8>>),
    ?assertEqual(ro, Ro),
    {ok, Pl} = translation_detect:detect(<<"na porozumieniu, na ³±czeniu si³ i ¶rodków. Dlatego szukam ludzi, którzy"/utf8>>),
    ?assertEqual(pl, Pl).

detect_lang_test() ->
    {ok, De} = translation_detect:detect(<<"sagt Hühsam das war bei Über eine Annonce in einem Frankfurter der Töpfer ein. Anhand von gefundenen gut kennt, hatte ihm die wahren Tatsachen Sechzehn Adorno-Schüler erinnern und daß ein Weiterdenken der Theorie für ihre Festlegung sind drei Jahre Erschütterung Einblick in die Abhängigkeit der Bauarbeiten sei"/utf8>>),
    ?assertEqual(de,De),
    {ok, Fi} = translation_detect:detect(<<"koulun arkistoihin pölyttymään, vaan nuoret saavat itse vaikuttaa ajatustensa eteenpäinviemiseen esimerkiksi"/utf8>>),
    ?assertEqual(fi, Fi),
    {ok, Hu} = translation_detect:detect(<<"esôzéseket egy kissé túlméretezte, ebbôl kifolyólag a Földet egy hatalmas árvíz mosta el"/utf8>>),
    ?assertEqual(hu, Hu),
    {ok, Fi} = translation_detect:detect(<<"koulun arkistoihin pölyttymään, vaan nuoret saavat itse vaikuttaa ajatustensa eteenpäinviemiseen esimerkiksi"/utf8>>),
    ?assertEqual(fi, Fi),
    {ok, Nl} = translation_detect:detect(<<"tegen de kabinetsplannen. Een speciaal in het leven geroepen Landelijk"/utf8>>),
    ?assertEqual(nl, Nl),
    {ok, Da} = translation_detect:detect(<<"viksomhed, 58 pct. har et arbejde eller er under uddannelse, 76 pct. forsørges ikke længere af Kolding"/utf8>>),
    ?assertEqual(da, Da),
    {ok, Cs} = translation_detect:detect(<<"datují rokem 1862.  Naprosto zakázán byl v pocitech smutku, beznadìje èi jiné"/utf8>>),
    ?assertEqual(cs, Cs),
    {ok, No} = translation_detect:detect(<<"hånd på den enda hvitere restaurant-duken med en bevegelse så forfinet"/utf8>>),
    ?assertEqual(no, No),
    {ok, Pt} = translation_detect:detect(<<"popular. Segundo o seu biógrafo, a Maria Adelaide auxiliava muita gente"/utf8>>),
    ?assertEqual(pt, Pt),
    {ok, Ar} = translation_detect:detect(<<"تكبّد المدنيون خسائر في الأرواح إبّان الحرب العالمية الثانية أكثر من أي حرب عرفها التاريخ، ويعزى السبب لتقليعة القصف الجوي على [[مدينة|المد"/utf8>>),
    ?assertEqual(ar, Ar),
    {ok, Jp} = translation_detect:detect(<<"このうちアジア・太平洋地域における戦局、すなわち日本が米英蘭に対して戦端を開いた["/utf8>>),
    ?assertEqual(ja, Jp),
    {ok, En} = translation_detect:detect(<<"TaffyDB finders looking nice so far!"/utf8>>),
    ?assertEqual(en, En).

rsc_insert_text_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Props = #{
        <<"is_published">> => true,
        <<"category_id">> => text,
        <<"title">> => <<"Hallo dit is een tekst"/utf8>>,
        <<"body">> => <<"Een langer verhaal om de taal beter detecteerbaar te maken."/utf8>>
    },
    {ok, Id} = m_rsc:insert(Props, Context),
    Language = m_rsc:p_no_acl(Id, language, Context),
    ?assertEqual([nl], Language).
