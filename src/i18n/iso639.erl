%%%-------------------------------------------------------------------
%%% Created     :  9 Mar 2004 by <tobbe@bluetail.com>
%%% Description : ISO 639 2- and 3-letter codes.
%%%-------------------------------------------------------------------
-module(iso639).
-export([lc2lang/1, all2lang/0, lc3lang/1, all3lang/0]).

%%%---------------------------------------------------------------------
%%% We should only be using 3-letter codes.
%%% However, a couple of languages seems to only have a 2-letter code.
%%%---------------------------------------------------------------------

lc3lang("abk") -> "Abkhazian";
lc3lang("ace") -> "Achinese";
lc3lang("ach") -> "Acoli";
lc3lang("ada") -> "Adangme";
lc3lang("aar") -> "Afar";
lc3lang("afh") -> "Afrihili";
lc3lang("afr") -> "Afrikaans";
lc3lang("afa") -> "Afro-Asiatic (Other)";
lc3lang("aka") -> "Akan";
lc3lang("akk") -> "Akkadian";
lc3lang("alb") -> "Albanian";
lc3lang("ale") -> "Aleut";
lc3lang("alg") -> "Algonquian languages";
lc3lang("tut") -> "Altaic (Other)";
lc3lang("amh") -> "Amharic";
lc3lang("apa") -> "Apache languages";
lc3lang("ara") -> "Arabic";
lc3lang("arc") -> "Aramaic";
lc3lang("arp") -> "Arapaho";
lc3lang("arn") -> "Araucanian";
lc3lang("arw") -> "Arawak";
lc3lang("arm") -> "Armenian";
lc3lang("art") -> "Artificial (Other)";
lc3lang("asm") -> "Assamese";
lc3lang("ath") -> "Athapascan languages";
lc3lang("map") -> "Austronesian (Other)";
lc3lang("ava") -> "Avaric";
lc3lang("ave") -> "Avestan";
lc3lang("awa") -> "Awadhi";
lc3lang("aym") -> "Aymara";
lc3lang("aze") -> "Azerbaijani";
lc3lang("nah") -> "Aztec";
lc3lang("ban") -> "Balinese";
lc3lang("bat") -> "Baltic (Other)";
lc3lang("bal") -> "Baluchi";
lc3lang("bam") -> "Bambara";
lc3lang("bai") -> "Bamileke languages";
lc3lang("bad") -> "Banda";
lc3lang("bnt") -> "Bantu (Other)";
lc3lang("bas") -> "Basa";
lc3lang("bak") -> "Bashkir";
lc3lang("baq") -> "Basque";
lc3lang("bej") -> "Beja";
lc3lang("bem") -> "Bemba";
lc3lang("ben") -> "Bengali";
lc3lang("ber") -> "Berber (Other)";
lc3lang("bho") -> "Bhojpuri";
lc3lang("bih") -> "Bihari";
lc3lang("bik") -> "Bikol";
lc3lang("bin") -> "Bini";
lc3lang("bis") -> "Bislama";
lc3lang("bra") -> "Braj";
lc3lang("bre") -> "Breton";
lc3lang("bug") -> "Buginese";
lc3lang("bul") -> "Bulgarian";
lc3lang("bua") -> "Buriat";
lc3lang("bur") -> "Burmese";
lc3lang("bel") -> "Byelorussian";
lc3lang("cad") -> "Caddo";
lc3lang("car") -> "Carib";
lc3lang("cat") -> "Catalan";
lc3lang("cau") -> "Caucasian (Other)";
lc3lang("ceb") -> "Cebuano";
lc3lang("cel") -> "Celtic (Other)";
lc3lang("cai") -> "Central American Indian (Other)";
lc3lang("chg") -> "Chagatai";
lc3lang("cha") -> "Chamorro";
lc3lang("che") -> "Chechen";
lc3lang("chr") -> "Cherokee";
lc3lang("chy") -> "Cheyenne";
lc3lang("chb") -> "Chibcha";
lc3lang("chi") -> "Chinese";
lc3lang("chn") -> "Chinook jargon";
lc3lang("cho") -> "Choctaw";
lc3lang("chu") -> "Church Slavic";
lc3lang("chv") -> "Chuvash";
lc3lang("cop") -> "Coptic";
lc3lang("cor") -> "Cornish";
lc3lang("cos") -> "Corsican";
lc3lang("cre") -> "Cree";
lc3lang("mus") -> "Creek";
lc3lang("crp") -> "Creoles and Pidgins (Other)";
lc3lang("cpe") -> "Creoles and Pidgins, English-based (Other)";
lc3lang("cpf") -> "Creoles and Pidgins, French-based (Other)";
lc3lang("cpp") -> "Creoles and Pidgins, Portuguese-based (Other)";
lc3lang("cus") -> "Cushitic (Other)";
lc3lang("hr")  -> "Croatian";
lc3lang("ces") -> "Czech";
lc3lang("dak") -> "Dakota";
lc3lang("dan") -> "Danish";
lc3lang("del") -> "Delaware";
lc3lang("din") -> "Dinka";
lc3lang("div") -> "Divehi";
lc3lang("doi") -> "Dogri";
lc3lang("dra") -> "Dravidian (Other)";
lc3lang("dua") -> "Duala";
lc3lang("dut") -> "Dutch";
lc3lang("dum") -> "Dutch, Middle (ca. 1050-1350)";
lc3lang("dyu") -> "Dyula";
lc3lang("dzo") -> "Dzongkha";
lc3lang("efi") -> "Efik";
lc3lang("egy") -> "Egyptian (Ancient)";
lc3lang("eka") -> "Ekajuk";
lc3lang("elx") -> "Elamite";
lc3lang("eng") -> "English";
lc3lang("enm") -> "English, Middle (ca. 1100-1500)";
lc3lang("ang") -> "English, Old (ca. 450-1100)";
lc3lang("esk") -> "Eskimo (Other)";
lc3lang("epo") -> "Esperanto";
lc3lang("est") -> "Estonian";
lc3lang("ewe") -> "Ewe";
lc3lang("ewo") -> "Ewondo";
lc3lang("fan") -> "Fang";
lc3lang("fat") -> "Fanti";
lc3lang("fao") -> "Faroese";
lc3lang("fij") -> "Fijian";
lc3lang("fin") -> "Finnish";
lc3lang("fiu") -> "Finno-Ugrian (Other)";
lc3lang("fon") -> "Fon";
lc3lang("fra") -> "French";
lc3lang("frm") -> "French, Middle (ca. 1400-1600)";
lc3lang("fro") -> "French, Old (842- ca. 1400)";
lc3lang("fry") -> "Frisian";
lc3lang("ful") -> "Fulah";
lc3lang("gaa") -> "Ga";
lc3lang("gae") -> " (Scots)";
lc3lang("glg") -> "Gallegan";
lc3lang("lug") -> "Ganda";
lc3lang("gay") -> "Gayo";
lc3lang("gez") -> "Geez";
lc3lang("geo") -> "Georgian";
lc3lang("deu") -> "German";
lc3lang("gmh") -> "German, Middle High (ca. 1050-1500)";
lc3lang("goh") -> "German, Old High (ca. 750-1050)";
lc3lang("gem") -> "Germanic (Other)";
lc3lang("gil") -> "Gilbertese";
lc3lang("gon") -> "Gondi";
lc3lang("got") -> "Gothic";
lc3lang("grb") -> "Grebo";
lc3lang("grc") -> "Greek, Ancient (to 1453)";
lc3lang("ell") -> "Greek, Modern (1453-)";
lc3lang("kal") -> "Greenlandic";
lc3lang("grn") -> "Guarani";
lc3lang("guj") -> "Gujarati";
lc3lang("hai") -> "Haida";
lc3lang("hau") -> "Hausa";
lc3lang("haw") -> "Hawaiian";
lc3lang("heb") -> "Hebrew";
lc3lang("her") -> "Herero";
lc3lang("hil") -> "Hiligaynon";
lc3lang("him") -> "Himachali";
lc3lang("hin") -> "Hindi";
lc3lang("hmo") -> "Hiri Motu";
lc3lang("hun") -> "Hungarian";
lc3lang("hup") -> "Hupa";
lc3lang("iba") -> "Iban";
lc3lang("ice") -> "Icelandic";
lc3lang("ibo") -> "Igbo";
lc3lang("ijo") -> "Ijo";
lc3lang("ilo") -> "Iloko";
lc3lang("inc") -> "Indic (Other)";
lc3lang("ine") -> "Indo-European (Other)";
lc3lang("ind") -> "Indonesian";
lc3lang("ina") -> "Interlingua (International Auxiliary language Association)";
%% ??? lc3lang("ine") -> "Interlingue";
lc3lang("iku") -> "Inuktitut";
lc3lang("ipk") -> "Inupiak";
lc3lang("ira") -> "Iranian (Other)";
lc3lang("gai") -> "Irish";
lc3lang("sga") -> "Irish, Old (to 900)";
lc3lang("mga") -> "Irish, Middle (900 - 1200)";
lc3lang("iro") -> "Iroquoian languages";
lc3lang("ita") -> "Italian";
lc3lang("jpn") -> "Japanese";
lc3lang("jav") -> "Javanese";
lc3lang("jrb") -> "Judeo-Arabic";
lc3lang("jpr") -> "Judeo-Persian";
lc3lang("kab") -> "Kabyle";
lc3lang("kac") -> "Kachin";
lc3lang("kam") -> "Kamba";
lc3lang("kan") -> "Kannada";
lc3lang("kau") -> "Kanuri";
lc3lang("kaa") -> "Kara-Kalpak";
lc3lang("kar") -> "Karen";
lc3lang("kas") -> "Kashmiri";
lc3lang("kaw") -> "Kawi";
lc3lang("kaz") -> "Kazakh";
lc3lang("kha") -> "Khasi";
lc3lang("khm") -> "Khmer";
lc3lang("khi") -> "Khoisan (Other)";
lc3lang("kho") -> "Khotanese";
lc3lang("kik") -> "Kikuyu";
lc3lang("kin") -> "Kinyarwanda";
lc3lang("kir") -> "Kirghiz";
lc3lang("kom") -> "Komi";
lc3lang("kon") -> "Kongo";
lc3lang("kok") -> "Konkani";
lc3lang("kor") -> "Korean";
lc3lang("kpe") -> "Kpelle";
lc3lang("kro") -> "Kru";
lc3lang("kua") -> "Kuanyama";
lc3lang("kum") -> "Kumyk";
lc3lang("kur") -> "Kurdish";
lc3lang("kru") -> "Kurukh";
lc3lang("kus") -> "Kusaie";
lc3lang("kut") -> "Kutenai";
lc3lang("lad") -> "Ladino";
lc3lang("lah") -> "Lahnda";
lc3lang("lam") -> "Lamba";
lc3lang("oci") -> "Langue d'Oc (post 1500)";
lc3lang("lao") -> "Lao";
lc3lang("lat") -> "Latin";
lc3lang("lav") -> "Latvian";
lc3lang("ltz") -> "Letzeburgesch";
lc3lang("lez") -> "Lezghian";
lc3lang("lin") -> "Lingala";
lc3lang("lit") -> "Lithuanian";
lc3lang("loz") -> "Lozi";
lc3lang("lub") -> "Luba-Katanga";
lc3lang("lui") -> "Luiseno";
lc3lang("lun") -> "Lunda";
lc3lang("luo") -> "Luo (Kenya and Tanzania)";
lc3lang("mac") -> "Macedonian";
lc3lang("mad") -> "Madurese";
lc3lang("mag") -> "Magahi";
lc3lang("mai") -> "Maithili";
lc3lang("mak") -> "Makasar";
lc3lang("mlg") -> "Malagasy";
lc3lang("may") -> "Malay";
lc3lang("mal") -> "Malayalam";
lc3lang("mlt") -> "Maltese";
lc3lang("man") -> "Mandingo";
lc3lang("mni") -> "Manipuri";
lc3lang("mno") -> "Manobo languages";
lc3lang("max") -> "Manx";
lc3lang("mao") -> "Maori";
lc3lang("mar") -> "Marathi";
lc3lang("chm") -> "Mari";
lc3lang("mah") -> "Marshall";
lc3lang("mwr") -> "Marwari";
lc3lang("mas") -> "Masai";
lc3lang("myn") -> "Mayan languages";
lc3lang("men") -> "Mende";
lc3lang("mic") -> "Micmac";
lc3lang("min") -> "Minangkabau";
lc3lang("mis") -> "Miscellaneous (Other)";
lc3lang("moh") -> "Mohawk";
lc3lang("mol") -> "Moldavian";
lc3lang("mkh") -> "Mon-Kmer (Other)";
lc3lang("lol") -> "Mongo";
lc3lang("mon") -> "Mongolian";
lc3lang("mos") -> "Mossi";
lc3lang("mul") -> "Multiple languages";
lc3lang("mun") -> "Munda languages";
lc3lang("nau") -> "Nauru";
lc3lang("nav") -> "Navajo";
lc3lang("nde") -> "Ndebele, North";
lc3lang("nbl") -> "Ndebele, South";
lc3lang("ndo") -> "Ndongo";
lc3lang("nep") -> "Nepali";
lc3lang("new") -> "Newari";
lc3lang("nic") -> "Niger-Kordofanian (Other)";
lc3lang("ssa") -> "Nilo-Saharan (Other)";
lc3lang("niu") -> "Niuean";
lc3lang("non") -> "Norse, Old";
lc3lang("nai") -> "North American Indian (Other)";
lc3lang("nor") -> "Norwegian";
lc3lang("nno") -> "Norwegian (Nynorsk)";
lc3lang("nub") -> "Nubian languages";
lc3lang("nym") -> "Nyamwezi";
lc3lang("nya") -> "Nyanja";
lc3lang("nyn") -> "Nyankole";
lc3lang("nyo") -> "Nyoro";
lc3lang("nzi") -> "Nzima";
lc3lang("oji") -> "Ojibwa";
lc3lang("ori") -> "Oriya";
lc3lang("orm") -> "Oromo";
lc3lang("osa") -> "Osage";
lc3lang("oss") -> "Ossetic";
lc3lang("oto") -> "Otomian languages";
lc3lang("pal") -> "Pahlavi";
lc3lang("pau") -> "Palauan";
lc3lang("pli") -> "Pali";
lc3lang("pam") -> "Pampanga";
lc3lang("pag") -> "Pangasinan";
lc3lang("pan") -> "Panjabi";
lc3lang("pap") -> "Papiamento";
lc3lang("paa") -> "Papuan-Australian (Other)";
lc3lang("fas") -> "Persian";
lc3lang("peo") -> "Persian, Old (ca 600 - 400 B.C.)";
lc3lang("phn") -> "Phoenician";
lc3lang("pol") -> "Polish";
lc3lang("pon") -> "Ponape";
lc3lang("por") -> "Portuguese";
lc3lang("pra") -> "Prakrit languages";
lc3lang("pro") -> "Provencal, Old (to 1500)";
lc3lang("pus") -> "Pushto";
lc3lang("que") -> "Quechua";
lc3lang("roh") -> "Rhaeto-Romance";
lc3lang("raj") -> "Rajasthani";
lc3lang("rar") -> "Rarotongan";
lc3lang("roa") -> "Romance (Other)";
lc3lang("ron") -> "Romanian";
lc3lang("rom") -> "Romany";
lc3lang("run") -> "Rundi";
lc3lang("rus") -> "Russian";
lc3lang("sal") -> "Salishan languages";
lc3lang("sam") -> "Samaritan Aramaic";
lc3lang("smi") -> "Sami languages";
lc3lang("smo") -> "Samoan";
lc3lang("sad") -> "Sandawe";
lc3lang("sag") -> "Sango";
lc3lang("san") -> "Sanskrit";
lc3lang("srd") -> "Sardinian";
lc3lang("sco") -> "Scots";
lc3lang("sel") -> "Selkup";
lc3lang("sem") -> "Semitic (Other)";
lc3lang("sr")  -> "Serbian";
lc3lang("scr") -> "Serbo-Croatian";
lc3lang("srr") -> "Serer";
lc3lang("shn") -> "Shan";
lc3lang("sna") -> "Shona";
lc3lang("sid") -> "Sidamo";
lc3lang("bla") -> "Siksika";
lc3lang("snd") -> "Sindhi";
lc3lang("sin") -> "Singhalese";
lc3lang("sit") -> "Sino-Tibetan (Other)";
lc3lang("sio") -> "Siouan languages";
lc3lang("sla") -> "Slavic (Other)";
lc3lang("ssw") -> "Siswant";
lc3lang("slk") -> "Slovak";
lc3lang("slv") -> "Slovenian";
lc3lang("sog") -> "Sogdian";
lc3lang("som") -> "Somali";
lc3lang("son") -> "Songhai";
lc3lang("wen") -> "Sorbian languages";
lc3lang("nso") -> "Sotho, Northern";
lc3lang("sot") -> "Sotho, Southern";
lc3lang("sai") -> "South American Indian (Other)";
lc3lang("esl") -> "Spanish";
lc3lang("suk") -> "Sukuma";
lc3lang("sux") -> "Sumerian";
lc3lang("sun") -> "Sudanese";
lc3lang("sus") -> "Susu";
lc3lang("swa") -> "Swahili";
%%% ??? same as 'Siswant' lc3lang("ssw") -> "Swazi";
lc3lang("sve") -> "Swedish";
lc3lang("syr") -> "Syriac";
lc3lang("tgl") -> "Tagalog";
lc3lang("tah") -> "Tahitian";
lc3lang("tgk") -> "Tajik";
lc3lang("tmh") -> "Tamashek";
lc3lang("tam") -> "Tamil";
lc3lang("tat") -> "Tatar";
lc3lang("tel") -> "Telugu";
lc3lang("ter") -> "Tereno";
lc3lang("tha") -> "Thai";
lc3lang("bod") -> "Tibetan";
lc3lang("tig") -> "Tigre";
lc3lang("tir") -> "Tigrinya";
lc3lang("tem") -> "Timne";
lc3lang("tiv") -> "Tivi";
lc3lang("tli") -> "Tlingit";
lc3lang("tog") -> "Tonga (Nyasa)";
lc3lang("ton") -> "Tonga (Tonga Islands)";
lc3lang("tru") -> "Truk";
lc3lang("tsi") -> "Tsimshian";
lc3lang("tso") -> "Tsonga";
lc3lang("tsn") -> "Tswana";
lc3lang("tum") -> "Tumbuka";
lc3lang("tur") -> "Turkish";
lc3lang("ota") -> "Turkish, Ottoman (1500 - 1928)";
lc3lang("tuk") -> "Turkmen";
lc3lang("tyv") -> "Tuvinian";
lc3lang("twi") -> "Twi";
lc3lang("uga") -> "Ugaritic";
lc3lang("uig") -> "Uighur";
lc3lang("ukr") -> "Ukrainian";
lc3lang("umb") -> "Umbundu";
lc3lang("und") -> "Undetermined";
lc3lang("urd") -> "Urdu";
lc3lang("uzb") -> "Uzbek";
lc3lang("vai") -> "Vai";
lc3lang("ven") -> "Venda";
lc3lang("vie") -> "Vietnamese";
lc3lang("vol") -> "Volapük";
lc3lang("vot") -> "Votic";
lc3lang("wak") -> "Wakashan languages";
lc3lang("wal") -> "Walamo";
lc3lang("war") -> "Waray";
lc3lang("was") -> "Washo";
lc3lang("cym") -> "Welsh";
lc3lang("wol") -> "Wolof";
lc3lang("xho") -> "Xhosa";
lc3lang("sah") -> "Yakut";
lc3lang("yao") -> "Yao";
lc3lang("yap") -> "Yap";
lc3lang("yid") -> "Yiddish";
lc3lang("yor") -> "Yoruba";
lc3lang("zap") -> "Zapotec";
lc3lang("zen") -> "Zenaga";
lc3lang("zha") -> "Zhuang";
lc3lang("zul") -> "Zulu";
lc3lang("zun") -> "Zuni";
lc3lang(_)     -> "".


all3lang() ->
    [{"abk", "Abkhazian"},
     {"ace", "Achinese"},
     {"ach", "Acoli"},
     {"ada", "Adangme"},
     {"aar", "Afar"},
     {"afh", "Afrihili"},
     {"afr", "Afrikaans"},
     {"afa", "Afro-Asiatic (Other)"},
     {"aka", "Akan"},
     {"akk", "Akkadian"},
     {"alb", "Albanian"},
     {"ale", "Aleut"},
     {"alg", "Algonquian languages"},
     {"tut", "Altaic (Other)"},
     {"amh", "Amharic"},
     {"apa", "Apache languages"},
     {"ara", "Arabic"},
     {"arc", "Aramaic"},
     {"arp", "Arapaho"},
     {"arn", "Araucanian"},
     {"arw", "Arawak"},
     {"arm", "Armenian"},
     {"art", "Artificial (Other)"},
     {"asm", "Assamese"},
     {"ath", "Athapascan languages"},
     {"map", "Austronesian (Other)"},
     {"ava", "Avaric"},
     {"ave", "Avestan"},
     {"awa", "Awadhi"},
     {"aym", "Aymara"},
     {"aze", "Azerbaijani"},
     {"nah", "Aztec"},
     {"ban", "Balinese"},
     {"bat", "Baltic (Other)"},
     {"bal", "Baluchi"},
     {"bam", "Bambara"},
     {"bai", "Bamileke languages"},
     {"bad", "Banda"},
     {"bnt", "Bantu (Other)"},
     {"bas", "Basa"},
     {"bak", "Bashkir"},
     {"baq", "Basque"},
     {"bej", "Beja"},
     {"bem", "Bemba"},
     {"ben", "Bengali"},
     {"ber", "Berber (Other)"},
     {"bho", "Bhojpuri"},
     {"bih", "Bihari"},
     {"bik", "Bikol"},
     {"bin", "Bini"},
     {"bis", "Bislama"},
     {"bra", "Braj"},
     {"bre", "Breton"},
     {"bug", "Buginese"},
     {"bul", "Bulgarian"},
     {"bua", "Buriat"},
     {"bur", "Burmese"},
     {"bel", "Byelorussian"},
     {"cad", "Caddo"},
     {"car", "Carib"},
     {"cat", "Catalan"},
     {"cau", "Caucasian (Other)"},
     {"ceb", "Cebuano"},
     {"cel", "Celtic (Other)"},
     {"cai", "Central American Indian (Other)"},
     {"chg", "Chagatai"},
     {"cha", "Chamorro"},
     {"che", "Chechen"},
     {"chr", "Cherokee"},
     {"chy", "Cheyenne"},
     {"chb", "Chibcha"},
     {"chi", "Chinese"},
     {"chn", "Chinook jargon"},
     {"cho", "Choctaw"},
     {"chu", "Church Slavic"},
     {"chv", "Chuvash"},
     {"cop", "Coptic"},
     {"cor", "Cornish"},
     {"cos", "Corsican"},
     {"cre", "Cree"},
     {"mus", "Creek"},
     {"crp", "Creoles and Pidgins (Other)"},
     {"cpe", "Creoles and Pidgins, English-based (Other)"},
     {"cpf", "Creoles and Pidgins, French-based (Other)"},
     {"cpp", "Creoles and Pidgins, Portuguese-based (Other)"},
     {"cus", "Cushitic (Other)"},
     {"hr", "Croatian"},
     {"ces", "Czech"},
     {"dak", "Dakota"},
     {"dan", "Danish"},
     {"del", "Delaware"},
     {"din", "Dinka"},
     {"div", "Divehi"},
     {"doi", "Dogri"},
     {"dra", "Dravidian (Other)"},
     {"dua", "Duala"},
     {"dut", "Dutch"},
     {"dum", "Dutch, Middle (ca. 1050-1350)"},
     {"dyu", "Dyula"},
     {"dzo", "Dzongkha"},
     {"efi", "Efik"},
     {"egy", "Egyptian (Ancient)"},
     {"eka", "Ekajuk"},
     {"elx", "Elamite"},
     {"eng", "English"},
     {"enm", "English, Middle (ca. 1100-1500)"},
     {"ang", "English, Old (ca. 450-1100)"},
     {"esk", "Eskimo (Other)"},
     {"epo", "Esperanto"},
     {"est", "Estonian"},
     {"ewe", "Ewe"},
     {"ewo", "Ewondo"},
     {"fan", "Fang"},
     {"fat", "Fanti"},
     {"fao", "Faroese"},
     {"fij", "Fijian"},
     {"fin", "Finnish"},
     {"fiu", "Finno-Ugrian (Other)"},
     {"fon", "Fon"},
     {"fra", "French"},
     {"frm", "French, Middle (ca. 1400-1600)"},
     {"fro", "French, Old (842- ca. 1400)"},
     {"fry", "Frisian"},
     {"ful", "Fulah"},
     {"gaa", "Ga"},
     {"gae", " (Scots)"},
     {"glg", "Gallegan"},
     {"lug", "Ganda"},
     {"gay", "Gayo"},
     {"gez", "Geez"},
     {"geo", "Georgian"},
     {"deu", "German"},
     {"gmh", "German, Middle High (ca. 1050-1500)"},
     {"goh", "German, Old High (ca. 750-1050)"},
     {"gem", "Germanic (Other)"},
     {"gil", "Gilbertese"},
     {"gon", "Gondi"},
     {"got", "Gothic"},
     {"grb", "Grebo"},
     {"grc", "Greek, Ancient (to 1453)"},
     {"ell", "Greek, Modern (1453-)"},
     {"kal", "Greenlandic"},
     {"grn", "Guarani"},
     {"guj", "Gujarati"},
     {"hai", "Haida"},
     {"hau", "Hausa"},
     {"haw", "Hawaiian"},
     {"heb", "Hebrew"},
     {"her", "Herero"},
     {"hil", "Hiligaynon"},
     {"him", "Himachali"},
     {"hin", "Hindi"},
     {"hmo", "Hiri Motu"},
     {"hun", "Hungarian"},
     {"hup", "Hupa"},
     {"iba", "Iban"},
     {"ice", "Icelandic"},
     {"ibo", "Igbo"},
     {"ijo", "Ijo"},
     {"ilo", "Iloko"},
     {"inc", "Indic (Other)"},
     {"ine", "Indo-European (Other)"},
     {"ind", "Indonesian"},
     {"ina", "Interlingua (International Auxiliary language Association)"},
     {"ine", "Interlingue"},
     {"iku", "Inuktitut"},
     {"ipk", "Inupiak"},
     {"ira", "Iranian (Other)"},
     {"gai", "Irish"},
     {"sga", "Irish, Old (to 900)"},
     {"mga", "Irish, Middle (900 - 1200)"},
     {"iro", "Iroquoian languages"},
     {"ita", "Italian"},
     {"jpn", "Japanese"},
     {"jav", "Javanese"},
     {"jrb", "Judeo-Arabic"},
     {"jpr", "Judeo-Persian"},
     {"kab", "Kabyle"},
     {"kac", "Kachin"},
     {"kam", "Kamba"},
     {"kan", "Kannada"},
     {"kau", "Kanuri"},
     {"kaa", "Kara-Kalpak"},
     {"kar", "Karen"},
     {"kas", "Kashmiri"},
     {"kaw", "Kawi"},
     {"kaz", "Kazakh"},
     {"kha", "Khasi"},
     {"khm", "Khmer"},
     {"khi", "Khoisan (Other)"},
     {"kho", "Khotanese"},
     {"kik", "Kikuyu"},
     {"kin", "Kinyarwanda"},
     {"kir", "Kirghiz"},
     {"kom", "Komi"},
     {"kon", "Kongo"},
     {"kok", "Konkani"},
     {"kor", "Korean"},
     {"kpe", "Kpelle"},
     {"kro", "Kru"},
     {"kua", "Kuanyama"},
     {"kum", "Kumyk"},
     {"kur", "Kurdish"},
     {"kru", "Kurukh"},
     {"kus", "Kusaie"},
     {"kut", "Kutenai"},
     {"lad", "Ladino"},
     {"lah", "Lahnda"},
     {"lam", "Lamba"},
     {"oci", "Langue d'Oc (post 1500)"},
     {"lao", "Lao"},
     {"lat", "Latin"},
     {"lav", "Latvian"},
     {"ltz", "Letzeburgesch"},
     {"lez", "Lezghian"},
     {"lin", "Lingala"},
     {"lit", "Lithuanian"},
     {"loz", "Lozi"},
     {"lub", "Luba-Katanga"},
     {"lui", "Luiseno"},
     {"lun", "Lunda"},
     {"luo", "Luo (Kenya and Tanzania)"},
     {"mac", "Macedonian"},
     {"mad", "Madurese"},
     {"mag", "Magahi"},
     {"mai", "Maithili"},
     {"mak", "Makasar"},
     {"mlg", "Malagasy"},
     {"may", "Malay"},
     {"mal", "Malayalam"},
     {"mlt", "Maltese"},
     {"man", "Mandingo"},
     {"mni", "Manipuri"},
     {"mno", "Manobo languages"},
     {"max", "Manx"},
     {"mao", "Maori"},
     {"mar", "Marathi"},
     {"chm", "Mari"},
     {"mah", "Marshall"},
     {"mwr", "Marwari"},
     {"mas", "Masai"},
     {"myn", "Mayan languages"},
     {"men", "Mende"},
     {"mic", "Micmac"},
     {"min", "Minangkabau"},
     {"mis", "Miscellaneous (Other)"},
     {"moh", "Mohawk"},
     {"mol", "Moldavian"},
     {"mkh", "Mon-Kmer (Other)"},
     {"lol", "Mongo"},
     {"mon", "Mongolian"},
     {"mos", "Mossi"},
     {"mul", "Multiple languages"},
     {"mun", "Munda languages"},
     {"nau", "Nauru"},
     {"nav", "Navajo"},
     {"nde", "Ndebele, North"},
     {"nbl", "Ndebele, South"},
     {"ndo", "Ndongo"},
     {"nep", "Nepali"},
     {"new", "Newari"},
     {"nic", "Niger-Kordofanian (Other)"},
     {"ssa", "Nilo-Saharan (Other)"},
     {"niu", "Niuean"},
     {"non", "Norse, Old"},
     {"nai", "North American Indian (Other)"},
     {"nor", "Norwegian"},
     {"nno", "Norwegian (Nynorsk)"},
     {"nub", "Nubian languages"},
     {"nym", "Nyamwezi"},
     {"nya", "Nyanja"},
     {"nyn", "Nyankole"},
     {"nyo", "Nyoro"},
     {"nzi", "Nzima"},
     {"oji", "Ojibwa"},
     {"ori", "Oriya"},
     {"orm", "Oromo"},
     {"osa", "Osage"},
     {"oss", "Ossetic"},
     {"oto", "Otomian languages"},
     {"pal", "Pahlavi"},
     {"pau", "Palauan"},
     {"pli", "Pali"},
     {"pam", "Pampanga"},
     {"pag", "Pangasinan"},
     {"pan", "Panjabi"},
     {"pap", "Papiamento"},
     {"paa", "Papuan-Australian (Other)"},
     {"fas", "Persian"},
     {"peo", "Persian, Old (ca 600 - 400 B.C.)"},
     {"phn", "Phoenician"},
     {"pol", "Polish"},
     {"pon", "Ponape"},
     {"por", "Portuguese"},
     {"pra", "Prakrit languages"},
     {"pro", "Provencal, Old (to 1500)"},
     {"pus", "Pushto"},
     {"que", "Quechua"},
     {"roh", "Rhaeto-Romance"},
     {"raj", "Rajasthani"},
     {"rar", "Rarotongan"},
     {"roa", "Romance (Other)"},
     {"ron", "Romanian"},
     {"rom", "Romany"},
     {"run", "Rundi"},
     {"rus", "Russian"},
     {"sal", "Salishan languages"},
     {"sam", "Samaritan Aramaic"},
     {"smi", "Sami languages"},
     {"smo", "Samoan"},
     {"sad", "Sandawe"},
     {"sag", "Sango"},
     {"san", "Sanskrit"},
     {"srd", "Sardinian"},
     {"sco", "Scots"},
     {"sel", "Selkup"},
     {"sem", "Semitic (Other)"},
     {"sr", "Serbian"},
     {"scr", "Serbo-Croatian"},
     {"srr", "Serer"},
     {"shn", "Shan"},
     {"sna", "Shona"},
     {"sid", "Sidamo"},
     {"bla", "Siksika"},
     {"snd", "Sindhi"},
     {"sin", "Singhalese"},
     {"sit", "Sino-Tibetan (Other)"},
     {"sio", "Siouan languages"},
     {"sla", "Slavic (Other)"},
     {"ssw", "Siswant"},
     {"slk", "Slovak"},
     {"slv", "Slovenian"},
     {"sog", "Sogdian"},
     {"som", "Somali"},
     {"son", "Songhai"},
     {"wen", "Sorbian languages"},
     {"nso", "Sotho, Northern"},
     {"sot", "Sotho, Southern"},
     {"sai", "South American Indian (Other)"},
     {"esl", "Spanish"},
     {"suk", "Sukuma"},
     {"sux", "Sumerian"},
     {"sun", "Sudanese"},
     {"sus", "Susu"},
     {"swa", "Swahili"},
     {"ssw", "Swazi"},
     {"sve", "Swedish"},
     {"syr", "Syriac"},
     {"tgl", "Tagalog"},
     {"tah", "Tahitian"},
     {"tgk", "Tajik"},
     {"tmh", "Tamashek"},
     {"tam", "Tamil"},
     {"tat", "Tatar"},
     {"tel", "Telugu"},
     {"ter", "Tereno"},
     {"tha", "Thai"},
     {"bod", "Tibetan"},
     {"tig", "Tigre"},
     {"tir", "Tigrinya"},
     {"tem", "Timne"},
     {"tiv", "Tivi"},
     {"tli", "Tlingit"},
     {"tog", "Tonga (Nyasa)"},
     {"ton", "Tonga (Tonga Islands)"},
     {"tru", "Truk"},
     {"tsi", "Tsimshian"},
     {"tso", "Tsonga"},
     {"tsn", "Tswana"},
     {"tum", "Tumbuka"},
     {"tur", "Turkish"},
     {"ota", "Turkish, Ottoman (1500 - 1928)"},
     {"tuk", "Turkmen"},
     {"tyv", "Tuvinian"},
     {"twi", "Twi"},
     {"uga", "Ugaritic"},
     {"uig", "Uighur"},
     {"ukr", "Ukrainian"},
     {"umb", "Umbundu"},
     {"und", "Undetermined"},
     {"urd", "Urdu"},
     {"uzb", "Uzbek"},
     {"vai", "Vai"},
     {"ven", "Venda"},
     {"vie", "Vietnamese"},
     {"vol", "Volapük"},
     {"vot", "Votic"},
     {"wak", "Wakashan languages"},
     {"wal", "Walamo"},
     {"war", "Waray"},
     {"was", "Washo"},
     {"cym", "Welsh"},
     {"wol", "Wolof"},
     {"xho", "Xhosa"},
     {"sah", "Yakut"},
     {"yao", "Yao"},
     {"yap", "Yap"},
     {"yid", "Yiddish"},
     {"yor", "Yoruba"},
     {"zap", "Zapotec"},
     {"zen", "Zenaga"},
     {"zha", "Zhuang"},
     {"zul", "Zulu"},
     {"zun", "Zuni"}].




lc2lang("aa") -> "Afar";
lc2lang("ab") -> "Abkhazian";
lc2lang("ae") -> "Avestan";
lc2lang("af") -> "Afrikaans";
lc2lang("am") -> "Amharic";
lc2lang("ar") -> "Arabic";
lc2lang("as") -> "Assamese";
lc2lang("ay") -> "Aymara";
lc2lang("az") -> "Azerbaijani";
lc2lang("ba") -> "Bashkir";
lc2lang("be") -> "Byelorussian; Belarusian";
lc2lang("bg") -> "Bulgarian";
lc2lang("bh") -> "Bihari";
lc2lang("bi") -> "Bislama";
lc2lang("bn") -> "Bengali; Bangla";
lc2lang("bo") -> "Tibetan";
lc2lang("br") -> "Breton";
lc2lang("bs") -> "Bosnian";
lc2lang("ca") -> "Catalan";
lc2lang("ce") -> "Chechen";
lc2lang("ch") -> "Chamorro";
lc2lang("co") -> "Corsican";
lc2lang("cs") -> "Czech";
lc2lang("cu") -> "Church Slavic";
lc2lang("cv") -> "Chuvash";
lc2lang("cy") -> "Welsh";
lc2lang("da") -> "Danish";
lc2lang("de") -> "German";
lc2lang("dz") -> "Dzongkha; Bhutani";
lc2lang("el") -> "Greek";
lc2lang("en") -> "English";
lc2lang("eo") -> "Esperanto";
lc2lang("es") -> "Spanish";
lc2lang("et") -> "Estonian";
lc2lang("eu") -> "Basque";
lc2lang("fa") -> "Persian";
lc2lang("fi") -> "Finnish";
lc2lang("fj") -> "Fijian; Fiji";
lc2lang("fo") -> "Faroese";
lc2lang("fr") -> "French";
lc2lang("fy") -> "Frisian";
lc2lang("ga") -> "Irish";
lc2lang("gd") -> "Scots; Gaelic";
lc2lang("gl") -> "Gallegan; Galician";
lc2lang("gn") -> "Guarani";
lc2lang("gu") -> "Gujarati";
lc2lang("gv") -> "Manx";
lc2lang("ha") -> "Hausa (?)";
lc2lang("he") -> "Hebrew (formerly iw)";
lc2lang("hi") -> "Hindi";
lc2lang("ho") -> "Hiri Motu";
lc2lang("hr") -> "Croatian";
lc2lang("hu") -> "Hungarian";
lc2lang("hy") -> "Armenian";
lc2lang("hz") -> "Herero";
lc2lang("ia") -> "Interlingua";
lc2lang("id") -> "Indonesian (formerly in)";
lc2lang("ie") -> "Interlingue";
lc2lang("ik") -> "Inupiak";
lc2lang("io") -> "Ido";
lc2lang("is") -> "Icelandic";
lc2lang("it") -> "Italian";
lc2lang("iu") -> "Inuktitut";
lc2lang("ja") -> "Japanese";
lc2lang("jv") -> "Javanese";
lc2lang("ka") -> "Georgian";
lc2lang("ki") -> "Kikuyu";
lc2lang("kj") -> "Kuanyama";
lc2lang("kk") -> "Kazakh";
lc2lang("kl") -> "Kalaallisut; Greenlandic";
lc2lang("km") -> "Khmer; Cambodian";
lc2lang("kn") -> "Kannada";
lc2lang("ko") -> "Korean";
lc2lang("ks") -> "Kashmiri";
lc2lang("ku") -> "Kurdish";
lc2lang("kv") -> "Komi";
lc2lang("kw") -> "Cornish";
lc2lang("ky") -> "Kirghiz";
lc2lang("la") -> "Latin";
lc2lang("lb") -> "Letzeburgesch";
lc2lang("ln") -> "Lingala";
lc2lang("lo") -> "Lao; Laotian";
lc2lang("lt") -> "Lithuanian";
lc2lang("lv") -> "Latvian; Lettish";
lc2lang("mg") -> "Malagasy";
lc2lang("mh") -> "Marshall";
lc2lang("mi") -> "Maori";
lc2lang("mk") -> "Macedonian";
lc2lang("ml") -> "Malayalam";
lc2lang("mn") -> "Mongolian";
lc2lang("mo") -> "Moldavian";
lc2lang("mr") -> "Marathi";
lc2lang("ms") -> "Malay";
lc2lang("mt") -> "Maltese";
lc2lang("my") -> "Burmese";
lc2lang("na") -> "Nauru";
lc2lang("nb") -> "Norwegian Bokmål";
lc2lang("nd") -> "Ndebele, North";
lc2lang("ne") -> "Nepali";
lc2lang("ng") -> "Ndonga";
lc2lang("nl") -> "Dutch";
lc2lang("nn") -> "Norwegian Nynorsk";
lc2lang("no") -> "Norwegian";
lc2lang("nr") -> "Ndebele, South";
lc2lang("nv") -> "Navajo";
lc2lang("ny") -> "Chichewa; Nyanja";
lc2lang("oc") -> "Occitan; Provençal";
lc2lang("om") -> "(Afan) Oromo";
lc2lang("or") -> "Oriya";
lc2lang("os") -> "Ossetian; Ossetic";
lc2lang("pa") -> "Panjabi; Punjabi";
lc2lang("pi") -> "Pali";
lc2lang("pl") -> "Polish";
lc2lang("ps") -> "Pashto, Pushto";
lc2lang("pt") -> "Portuguese";
lc2lang("qu") -> "Quechua";
lc2lang("rm") -> "Rhaeto-Romance";
lc2lang("rn") -> "Rundi; Kirundi";
lc2lang("ro") -> "Romanian";
lc2lang("ru") -> "Russian";
lc2lang("rw") -> "Kinyarwanda";
lc2lang("sa") -> "Sanskrit";
lc2lang("sc") -> "Sardinian";
lc2lang("sd") -> "Sindhi";
lc2lang("se") -> "Northern Sami";
lc2lang("sg") -> "Sango; Sangro";
lc2lang("si") -> "Sinhalese";
lc2lang("sk") -> "Slovak";
lc2lang("sl") -> "Slovenian";
lc2lang("sm") -> "Samoan";
lc2lang("sn") -> "Shona";
lc2lang("so") -> "Somali";
lc2lang("sq") -> "Albanian";
lc2lang("sr") -> "Serbian";
lc2lang("ss") -> "Swati; Siswati";
lc2lang("st") -> "Sesotho; Sotho, Southern";
lc2lang("su") -> "Sundanese";
lc2lang("sv") -> "Swedish";
lc2lang("sw") -> "Swahili";
lc2lang("ta") -> "Tamil";
lc2lang("te") -> "Telugu";
lc2lang("tg") -> "Tajik";
lc2lang("th") -> "Thai";
lc2lang("ti") -> "Tigrinya";
lc2lang("tk") -> "Turkmen";
lc2lang("tl") -> "Tagalog";
lc2lang("tn") -> "Tswana; Setswana";
lc2lang("to") -> "Tonga (?)";
lc2lang("tr") -> "Turkish";
lc2lang("ts") -> "Tsonga";
lc2lang("tt") -> "Tatar";
lc2lang("tw") -> "Twi";
lc2lang("ty") -> "Tahitian";
lc2lang("ug") -> "Uighur";
lc2lang("uk") -> "Ukrainian";
lc2lang("ur") -> "Urdu";
lc2lang("uz") -> "Uzbek";
lc2lang("vi") -> "Vietnamese";
lc2lang("vo") -> "Volapuk";
lc2lang("wa") -> "Walloon";
lc2lang("wo") -> "Wolof";
lc2lang("xh") -> "Xhosa";
lc2lang("yi") -> "Yiddish (formerly ji)";
lc2lang("yo") -> "Yoruba";
lc2lang("za") -> "Zhuang";
lc2lang("zh") -> "Chinese";
lc2lang("zu") -> "Zulu";
lc2lang(_)   -> "".


all2lang() ->
    [{"aa", "Afar"},
     {"ab", "Abkhazian"},
     {"ae", "Avestan"},
     {"af", "Afrikaans"},
     {"am", "Amharic"},
     {"ar", "Arabic"},
     {"as", "Assamese"},
     {"ay", "Aymara"},
     {"az", "Azerbaijani"},
     {"ba", "Bashkir"},
     {"be", "Byelorussian; Belarusian"},
     {"bg", "Bulgarian"},
     {"bh", "Bihari"},
     {"bi", "Bislama"},
     {"bn", "Bengali; Bangla"},
     {"bo", "Tibetan"},
     {"br", "Breton"},
     {"bs", "Bosnian"},
     {"ca", "Catalan"},
     {"ce", "Chechen"},
     {"ch", "Chamorro"},
     {"co", "Corsican"},
     {"cs", "Czech"},
     {"cu", "Church Slavic"},
     {"cv", "Chuvash"},
     {"cy", "Welsh"},
     {"da", "Danish"},
     {"de", "German"},
     {"dz", "Dzongkha; Bhutani"},
     {"el", "Greek"},
     {"en", "English"},
     {"eo", "Esperanto"},
     {"es", "Spanish"},
     {"et", "Estonian"},
     {"eu", "Basque"},
     {"fa", "Persian"},
     {"fi", "Finnish"},
     {"fj", "Fijian; Fiji"},
     {"fo", "Faroese"},
     {"fr", "French"},
     {"fy", "Frisian"},
     {"ga", "Irish"},
     {"gd", "Scots; Gaelic"},
     {"gl", "Gallegan; Galician"},
     {"gn", "Guarani"},
     {"gu", "Gujarati"},
     {"gv", "Manx"},
     {"ha", "Hausa (?)"},
     {"he", "Hebrew (formerly iw)"},
     {"hi", "Hindi"},
     {"ho", "Hiri Motu"},
     {"hr", "Croatian"},
     {"hu", "Hungarian"},
     {"hy", "Armenian"},
     {"hz", "Herero"},
     {"ia", "Interlingua"},
     {"id", "Indonesian (formerly in)"},
     {"ie", "Interlingue"},
     {"ik", "Inupiak"},
     {"io", "Ido"},
     {"is", "Icelandic"},
     {"it", "Italian"},
     {"iu", "Inuktitut"},
     {"ja", "Japanese"},
     {"jv", "Javanese"},
     {"ka", "Georgian"},
     {"ki", "Kikuyu"},
     {"kj", "Kuanyama"},
     {"kk", "Kazakh"},
     {"kl", "Kalaallisut; Greenlandic"},
     {"km", "Khmer; Cambodian"},
     {"kn", "Kannada"},
     {"ko", "Korean"},
     {"ks", "Kashmiri"},
     {"ku", "Kurdish"},
     {"kv", "Komi"},
     {"kw", "Cornish"},
     {"ky", "Kirghiz"},
     {"la", "Latin"},
     {"lb", "Letzeburgesch"},
     {"ln", "Lingala"},
     {"lo", "Lao; Laotian"},
     {"lt", "Lithuanian"},
     {"lv", "Latvian; Lettish"},
     {"mg", "Malagasy"},
     {"mh", "Marshall"},
     {"mi", "Maori"},
     {"mk", "Macedonian"},
     {"ml", "Malayalam"},
     {"mn", "Mongolian"},
     {"mo", "Moldavian"},
     {"mr", "Marathi"},
     {"ms", "Malay"},
     {"mt", "Maltese"},
     {"my", "Burmese"},
     {"na", "Nauru"},
     {"nb", "Norwegian Bokmål"},
     {"nd", "Ndebele, North"},
     {"ne", "Nepali"},
     {"ng", "Ndonga"},
     {"nl", "Dutch"},
     {"nn", "Norwegian Nynorsk"},
     {"no", "Norwegian"},
     {"nr", "Ndebele, South"},
     {"nv", "Navajo"},
     {"ny", "Chichewa; Nyanja"},
     {"oc", "Occitan; Provençal"},
     {"om", "(Afan) Oromo"},
     {"or", "Oriya"},
     {"os", "Ossetian; Ossetic"},
     {"pa", "Panjabi; Punjabi"},
     {"pi", "Pali"},
     {"pl", "Polish"},
     {"ps", "Pashto, Pushto"},
     {"pt", "Portuguese"},
     {"qu", "Quechua"},
     {"rm", "Rhaeto-Romance"},
     {"rn", "Rundi; Kirundi"},
     {"ro", "Romanian"},
     {"ru", "Russian"},
     {"rw", "Kinyarwanda"},
     {"sa", "Sanskrit"},
     {"sc", "Sardinian"},
     {"sd", "Sindhi"},
     {"se", "Northern Sami"},
     {"sg", "Sango; Sangro"},
     {"si", "Sinhalese"},
     {"sk", "Slovak"},
     {"sl", "Slovenian"},
     {"sm", "Samoan"},
     {"sn", "Shona"},
     {"so", "Somali"},
     {"sq", "Albanian"},
     {"sr", "Serbian"},
     {"ss", "Swati; Siswati"},
     {"st", "Sesotho; Sotho, Southern"},
     {"su", "Sundanese"},
     {"sv", "Swedish"},
     {"sw", "Swahili"},
     {"ta", "Tamil"},
     {"te", "Telugu"},
     {"tg", "Tajik"},
     {"th", "Thai"},
     {"ti", "Tigrinya"},
     {"tk", "Turkmen"},
     {"tl", "Tagalog"},
     {"tn", "Tswana; Setswana"},
     {"to", "Tonga (?)"},
     {"tr", "Turkish"},
     {"ts", "Tsonga"},
     {"tt", "Tatar"},
     {"tw", "Twi"},
     {"ty", "Tahitian"},
     {"ug", "Uighur"},
     {"uk", "Ukrainian"},
     {"ur", "Urdu"},
     {"uz", "Uzbek"},
     {"vi", "Vietnamese"},
     {"vo", "Volapuk"},
     {"wa", "Walloon"},
     {"wo", "Wolof"},
     {"xh", "Xhosa"},
     {"yi", "Yiddish (formerly ji)"},
     {"yo", "Yoruba"},
     {"za", "Zhuang"},
     {"zh", "Chinese"},
     {"zu", "Zulu"}].

