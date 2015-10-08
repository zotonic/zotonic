%%%-------------------------------------------------------------------
%%% Created     :  9 Mar 2004 by <tobbe@bluetail.com>
%%% Description : ISO 639 2- and 3-letter codes.
%%%-------------------------------------------------------------------
%% coding: utf-8

-module(iso639).
-export([lc2lang/1, all2lang/0, lc3lang/1, all3lang/0]).

%%%---------------------------------------------------------------------
%%% Zotonic only uses 2-letter codes.
%%% However, a couple of languages only have a 3-letter code.
%%%---------------------------------------------------------------------

%%%
%%% ============================== FOR R16 OR LATER ===========================
%%%

lc3lang("abk") -> <<"Abkhazian"/utf8>>;
lc3lang("ace") -> <<"Achinese"/utf8>>;
lc3lang("ach") -> <<"Acoli"/utf8>>;
lc3lang("ada") -> <<"Adangme"/utf8>>;
lc3lang("aar") -> <<"Afar"/utf8>>;
lc3lang("afh") -> <<"Afrihili"/utf8>>;
lc3lang("afr") -> <<"Afrikaans"/utf8>>;
lc3lang("afa") -> <<"Afro-Asiatic (Other)"/utf8>>;
lc3lang("aka") -> <<"Akan"/utf8>>;
lc3lang("akk") -> <<"Akkadian"/utf8>>;
lc3lang("alb") -> <<"Albanian"/utf8>>;
lc3lang("ale") -> <<"Aleut"/utf8>>;
lc3lang("alg") -> <<"Algonquian languages"/utf8>>;
lc3lang("tut") -> <<"Altaic (Other)"/utf8>>;
lc3lang("amh") -> <<"Amharic"/utf8>>;
lc3lang("apa") -> <<"Apache languages"/utf8>>;
lc3lang("ara") -> <<"Arabic"/utf8>>;
lc3lang("arc") -> <<"Aramaic"/utf8>>;
lc3lang("arp") -> <<"Arapaho"/utf8>>;
lc3lang("arn") -> <<"Araucanian"/utf8>>;
lc3lang("arw") -> <<"Arawak"/utf8>>;
lc3lang("arm") -> <<"Armenian"/utf8>>;
lc3lang("art") -> <<"Artificial (Other)"/utf8>>;
lc3lang("asm") -> <<"Assamese"/utf8>>;
lc3lang("ath") -> <<"Athapascan languages"/utf8>>;
lc3lang("map") -> <<"Austronesian (Other)"/utf8>>;
lc3lang("ava") -> <<"Avaric"/utf8>>;
lc3lang("ave") -> <<"Avestan"/utf8>>;
lc3lang("awa") -> <<"Awadhi"/utf8>>;
lc3lang("aym") -> <<"Aymara"/utf8>>;
lc3lang("aze") -> <<"Azerbaijani"/utf8>>;
lc3lang("nah") -> <<"Aztec"/utf8>>;
lc3lang("ban") -> <<"Balinese"/utf8>>;
lc3lang("bat") -> <<"Baltic (Other)"/utf8>>;
lc3lang("bal") -> <<"Baluchi"/utf8>>;
lc3lang("bam") -> <<"Bambara"/utf8>>;
lc3lang("bai") -> <<"Bamileke languages"/utf8>>;
lc3lang("bad") -> <<"Banda"/utf8>>;
lc3lang("bnt") -> <<"Bantu (Other)"/utf8>>;
lc3lang("bas") -> <<"Basa"/utf8>>;
lc3lang("bak") -> <<"Bashkir"/utf8>>;
lc3lang("baq") -> <<"Basque"/utf8>>;
lc3lang("bej") -> <<"Beja"/utf8>>;
lc3lang("bem") -> <<"Bemba"/utf8>>;
lc3lang("ben") -> <<"Bengali"/utf8>>;
lc3lang("ber") -> <<"Berber (Other)"/utf8>>;
lc3lang("bho") -> <<"Bhojpuri"/utf8>>;
lc3lang("bih") -> <<"Bihari"/utf8>>;
lc3lang("bik") -> <<"Bikol"/utf8>>;
lc3lang("bin") -> <<"Bini"/utf8>>;
lc3lang("bis") -> <<"Bislama"/utf8>>;
lc3lang("bra") -> <<"Braj"/utf8>>;
lc3lang("bre") -> <<"Breton"/utf8>>;
lc3lang("bug") -> <<"Buginese"/utf8>>;
lc3lang("bul") -> <<"Bulgarian"/utf8>>;
lc3lang("bua") -> <<"Buriat"/utf8>>;
lc3lang("bur") -> <<"Burmese"/utf8>>;
lc3lang("bel") -> <<"Byelorussian"/utf8>>;
lc3lang("cad") -> <<"Caddo"/utf8>>;
lc3lang("car") -> <<"Carib"/utf8>>;
lc3lang("cat") -> <<"Catalan"/utf8>>;
lc3lang("cau") -> <<"Caucasian (Other)"/utf8>>;
lc3lang("ceb") -> <<"Cebuano"/utf8>>;
lc3lang("cel") -> <<"Celtic (Other)"/utf8>>;
lc3lang("cai") -> <<"Central American Indian (Other)"/utf8>>;
lc3lang("chg") -> <<"Chagatai"/utf8>>;
lc3lang("cha") -> <<"Chamorro"/utf8>>;
lc3lang("che") -> <<"Chechen"/utf8>>;
lc3lang("chr") -> <<"Cherokee"/utf8>>;
lc3lang("chy") -> <<"Cheyenne"/utf8>>;
lc3lang("chb") -> <<"Chibcha"/utf8>>;
lc3lang("chi") -> <<"Chinese"/utf8>>;
lc3lang("chn") -> <<"Chinook jargon"/utf8>>;
lc3lang("cho") -> <<"Choctaw"/utf8>>;
lc3lang("chu") -> <<"Church Slavic"/utf8>>;
lc3lang("chv") -> <<"Chuvash"/utf8>>;
lc3lang("cop") -> <<"Coptic"/utf8>>;
lc3lang("cor") -> <<"Cornish"/utf8>>;
lc3lang("cos") -> <<"Corsican"/utf8>>;
lc3lang("cre") -> <<"Cree"/utf8>>;
lc3lang("mus") -> <<"Creek"/utf8>>;
lc3lang("crp") -> <<"Creoles and Pidgins (Other)"/utf8>>;
lc3lang("cpe") -> <<"Creoles and Pidgins, English-based (Other)"/utf8>>;
lc3lang("cpf") -> <<"Creoles and Pidgins, French-based (Other)"/utf8>>;
lc3lang("cpp") -> <<"Creoles and Pidgins, Portuguese-based (Other)"/utf8>>;
lc3lang("cus") -> <<"Cushitic (Other)"/utf8>>;
lc3lang("hr")  -> <<"Croatian"/utf8>>;
lc3lang("ces") -> <<"Czech"/utf8>>;
lc3lang("dak") -> <<"Dakota"/utf8>>;
lc3lang("dan") -> <<"Danish"/utf8>>;
lc3lang("del") -> <<"Delaware"/utf8>>;
lc3lang("din") -> <<"Dinka"/utf8>>;
lc3lang("div") -> <<"Divehi"/utf8>>;
lc3lang("doi") -> <<"Dogri"/utf8>>;
lc3lang("dra") -> <<"Dravidian (Other)"/utf8>>;
lc3lang("dua") -> <<"Duala"/utf8>>;
lc3lang("dut") -> <<"Dutch"/utf8>>;
lc3lang("dum") -> <<"Dutch, Middle (ca. 1050-1350)"/utf8>>;
lc3lang("dyu") -> <<"Dyula"/utf8>>;
lc3lang("dzo") -> <<"Dzongkha"/utf8>>;
lc3lang("efi") -> <<"Efik"/utf8>>;
lc3lang("egy") -> <<"Egyptian (Ancient)"/utf8>>;
lc3lang("eka") -> <<"Ekajuk"/utf8>>;
lc3lang("elx") -> <<"Elamite"/utf8>>;
lc3lang("eng") -> <<"English"/utf8>>;
lc3lang("enm") -> <<"English, Middle (ca. 1100-1500)"/utf8>>;
lc3lang("ang") -> <<"English, Old (ca. 450-1100)"/utf8>>;
lc3lang("esk") -> <<"Eskimo (Other)"/utf8>>;
lc3lang("epo") -> <<"Esperanto"/utf8>>;
lc3lang("est") -> <<"Estonian"/utf8>>;
lc3lang("ewe") -> <<"Ewe"/utf8>>;
lc3lang("ewo") -> <<"Ewondo"/utf8>>;
lc3lang("fan") -> <<"Fang"/utf8>>;
lc3lang("fat") -> <<"Fanti"/utf8>>;
lc3lang("fao") -> <<"Faroese"/utf8>>;
lc3lang("fij") -> <<"Fijian"/utf8>>;
lc3lang("fin") -> <<"Finnish"/utf8>>;
lc3lang("fiu") -> <<"Finno-Ugrian (Other)"/utf8>>;
lc3lang("fon") -> <<"Fon"/utf8>>;
lc3lang("fra") -> <<"French"/utf8>>;
lc3lang("frm") -> <<"French, Middle (ca. 1400-1600)"/utf8>>;
lc3lang("fro") -> <<"French, Old (842- ca. 1400)"/utf8>>;
lc3lang("fry") -> <<"Frisian"/utf8>>;
lc3lang("ful") -> <<"Fulah"/utf8>>;
lc3lang("gaa") -> <<"Ga"/utf8>>;
lc3lang("gae") -> <<" (Scots)"/utf8>>;
lc3lang("glg") -> <<"Gallegan"/utf8>>;
lc3lang("lug") -> <<"Ganda"/utf8>>;
lc3lang("gay") -> <<"Gayo"/utf8>>;
lc3lang("gez") -> <<"Geez"/utf8>>;
lc3lang("geo") -> <<"Georgian"/utf8>>;
lc3lang("deu") -> <<"German"/utf8>>;
lc3lang("gmh") -> <<"German, Middle High (ca. 1050-1500)"/utf8>>;
lc3lang("goh") -> <<"German, Old High (ca. 750-1050)"/utf8>>;
lc3lang("gem") -> <<"Germanic (Other)"/utf8>>;
lc3lang("gil") -> <<"Gilbertese"/utf8>>;
lc3lang("gon") -> <<"Gondi"/utf8>>;
lc3lang("got") -> <<"Gothic"/utf8>>;
lc3lang("grb") -> <<"Grebo"/utf8>>;
lc3lang("grc") -> <<"Greek, Ancient (to 1453)"/utf8>>;
lc3lang("ell") -> <<"Greek, Modern (1453-)"/utf8>>;
lc3lang("kal") -> <<"Greenlandic"/utf8>>;
lc3lang("grn") -> <<"Guarani"/utf8>>;
lc3lang("guj") -> <<"Gujarati"/utf8>>;
lc3lang("hai") -> <<"Haida"/utf8>>;
lc3lang("hau") -> <<"Hausa"/utf8>>;
lc3lang("haw") -> <<"Hawaiian"/utf8>>;
lc3lang("heb") -> <<"Hebrew"/utf8>>;
lc3lang("her") -> <<"Herero"/utf8>>;
lc3lang("hil") -> <<"Hiligaynon"/utf8>>;
lc3lang("him") -> <<"Himachali"/utf8>>;
lc3lang("hin") -> <<"Hindi"/utf8>>;
lc3lang("hmo") -> <<"Hiri Motu"/utf8>>;
lc3lang("hun") -> <<"Hungarian"/utf8>>;
lc3lang("hup") -> <<"Hupa"/utf8>>;
lc3lang("iba") -> <<"Iban"/utf8>>;
lc3lang("ice") -> <<"Icelandic"/utf8>>;
lc3lang("ibo") -> <<"Igbo"/utf8>>;
lc3lang("ijo") -> <<"Ijo"/utf8>>;
lc3lang("ilo") -> <<"Iloko"/utf8>>;
lc3lang("inc") -> <<"Indic (Other)"/utf8>>;
lc3lang("ine") -> <<"Indo-European (Other)"/utf8>>;
lc3lang("ind") -> <<"Indonesian"/utf8>>;
lc3lang("ina") -> <<"Interlingua (International Auxiliary language Association)"/utf8>>;
%% ??? lc3lang("ine") -> <<"Interlingue"/utf8>>;
lc3lang("iku") -> <<"Inuktitut"/utf8>>;
lc3lang("ipk") -> <<"Inupiak"/utf8>>;
lc3lang("ira") -> <<"Iranian (Other)"/utf8>>;
lc3lang("gai") -> <<"Irish"/utf8>>;
lc3lang("sga") -> <<"Irish, Old (to 900)"/utf8>>;
lc3lang("mga") -> <<"Irish, Middle (900 - 1200)"/utf8>>;
lc3lang("iro") -> <<"Iroquoian languages"/utf8>>;
lc3lang("ita") -> <<"Italian"/utf8>>;
lc3lang("jpn") -> <<"Japanese"/utf8>>;
lc3lang("jav") -> <<"Javanese"/utf8>>;
lc3lang("jrb") -> <<"Judeo-Arabic"/utf8>>;
lc3lang("jpr") -> <<"Judeo-Persian"/utf8>>;
lc3lang("kab") -> <<"Kabyle"/utf8>>;
lc3lang("kac") -> <<"Kachin"/utf8>>;
lc3lang("kam") -> <<"Kamba"/utf8>>;
lc3lang("kan") -> <<"Kannada"/utf8>>;
lc3lang("kau") -> <<"Kanuri"/utf8>>;
lc3lang("kaa") -> <<"Kara-Kalpak"/utf8>>;
lc3lang("kar") -> <<"Karen"/utf8>>;
lc3lang("kas") -> <<"Kashmiri"/utf8>>;
lc3lang("kaw") -> <<"Kawi"/utf8>>;
lc3lang("kaz") -> <<"Kazakh"/utf8>>;
lc3lang("kha") -> <<"Khasi"/utf8>>;
lc3lang("khm") -> <<"Khmer"/utf8>>;
lc3lang("khi") -> <<"Khoisan (Other)"/utf8>>;
lc3lang("kho") -> <<"Khotanese"/utf8>>;
lc3lang("kik") -> <<"Kikuyu"/utf8>>;
lc3lang("kin") -> <<"Kinyarwanda"/utf8>>;
lc3lang("kir") -> <<"Kirghiz"/utf8>>;
lc3lang("kom") -> <<"Komi"/utf8>>;
lc3lang("kon") -> <<"Kongo"/utf8>>;
lc3lang("kok") -> <<"Konkani"/utf8>>;
lc3lang("kor") -> <<"Korean"/utf8>>;
lc3lang("kpe") -> <<"Kpelle"/utf8>>;
lc3lang("kro") -> <<"Kru"/utf8>>;
lc3lang("kua") -> <<"Kuanyama"/utf8>>;
lc3lang("kum") -> <<"Kumyk"/utf8>>;
lc3lang("kur") -> <<"Kurdish"/utf8>>;
lc3lang("kru") -> <<"Kurukh"/utf8>>;
lc3lang("kus") -> <<"Kusaie"/utf8>>;
lc3lang("kut") -> <<"Kutenai"/utf8>>;
lc3lang("lad") -> <<"Ladino"/utf8>>;
lc3lang("lah") -> <<"Lahnda"/utf8>>;
lc3lang("lam") -> <<"Lamba"/utf8>>;
lc3lang("oci") -> <<"Langue d'Oc (post 1500)"/utf8>>;
lc3lang("lao") -> <<"Lao"/utf8>>;
lc3lang("lat") -> <<"Latin"/utf8>>;
lc3lang("lav") -> <<"Latvian"/utf8>>;
lc3lang("ltz") -> <<"Letzeburgesch"/utf8>>;
lc3lang("lez") -> <<"Lezghian"/utf8>>;
lc3lang("lin") -> <<"Lingala"/utf8>>;
lc3lang("lit") -> <<"Lithuanian"/utf8>>;
lc3lang("loz") -> <<"Lozi"/utf8>>;
lc3lang("lub") -> <<"Luba-Katanga"/utf8>>;
lc3lang("lui") -> <<"Luiseno"/utf8>>;
lc3lang("lun") -> <<"Lunda"/utf8>>;
lc3lang("luo") -> <<"Luo (Kenya and Tanzania)"/utf8>>;
lc3lang("mac") -> <<"Macedonian"/utf8>>;
lc3lang("mad") -> <<"Madurese"/utf8>>;
lc3lang("mag") -> <<"Magahi"/utf8>>;
lc3lang("mai") -> <<"Maithili"/utf8>>;
lc3lang("mak") -> <<"Makasar"/utf8>>;
lc3lang("mlg") -> <<"Malagasy"/utf8>>;
lc3lang("may") -> <<"Malay"/utf8>>;
lc3lang("mal") -> <<"Malayalam"/utf8>>;
lc3lang("mlt") -> <<"Maltese"/utf8>>;
lc3lang("man") -> <<"Mandingo"/utf8>>;
lc3lang("mni") -> <<"Manipuri"/utf8>>;
lc3lang("mno") -> <<"Manobo languages"/utf8>>;
lc3lang("max") -> <<"Manx"/utf8>>;
lc3lang("mao") -> <<"Maori"/utf8>>;
lc3lang("mar") -> <<"Marathi"/utf8>>;
lc3lang("chm") -> <<"Mari"/utf8>>;
lc3lang("mah") -> <<"Marshall"/utf8>>;
lc3lang("mwr") -> <<"Marwari"/utf8>>;
lc3lang("mas") -> <<"Masai"/utf8>>;
lc3lang("myn") -> <<"Mayan languages"/utf8>>;
lc3lang("men") -> <<"Mende"/utf8>>;
lc3lang("mic") -> <<"Micmac"/utf8>>;
lc3lang("min") -> <<"Minangkabau"/utf8>>;
lc3lang("mis") -> <<"Miscellaneous (Other)"/utf8>>;
lc3lang("moh") -> <<"Mohawk"/utf8>>;
lc3lang("mol") -> <<"Moldavian"/utf8>>;
lc3lang("mkh") -> <<"Mon-Kmer (Other)"/utf8>>;
lc3lang("lol") -> <<"Mongo"/utf8>>;
lc3lang("mon") -> <<"Mongolian"/utf8>>;
lc3lang("mos") -> <<"Mossi"/utf8>>;
lc3lang("mul") -> <<"Multiple languages"/utf8>>;
lc3lang("mun") -> <<"Munda languages"/utf8>>;
lc3lang("nau") -> <<"Nauru"/utf8>>;
lc3lang("nav") -> <<"Navajo"/utf8>>;
lc3lang("nde") -> <<"Ndebele, North"/utf8>>;
lc3lang("nbl") -> <<"Ndebele, South"/utf8>>;
lc3lang("ndo") -> <<"Ndongo"/utf8>>;
lc3lang("nep") -> <<"Nepali"/utf8>>;
lc3lang("new") -> <<"Newari"/utf8>>;
lc3lang("nic") -> <<"Niger-Kordofanian (Other)"/utf8>>;
lc3lang("ssa") -> <<"Nilo-Saharan (Other)"/utf8>>;
lc3lang("niu") -> <<"Niuean"/utf8>>;
lc3lang("non") -> <<"Norse, Old"/utf8>>;
lc3lang("nai") -> <<"North American Indian (Other)"/utf8>>;
lc3lang("nor") -> <<"Norwegian"/utf8>>;
lc3lang("nno") -> <<"Norwegian (Nynorsk)"/utf8>>;
lc3lang("nub") -> <<"Nubian languages"/utf8>>;
lc3lang("nym") -> <<"Nyamwezi"/utf8>>;
lc3lang("nya") -> <<"Nyanja"/utf8>>;
lc3lang("nyn") -> <<"Nyankole"/utf8>>;
lc3lang("nyo") -> <<"Nyoro"/utf8>>;
lc3lang("nzi") -> <<"Nzima"/utf8>>;
lc3lang("oji") -> <<"Ojibwa"/utf8>>;
lc3lang("ori") -> <<"Oriya"/utf8>>;
lc3lang("orm") -> <<"Oromo"/utf8>>;
lc3lang("osa") -> <<"Osage"/utf8>>;
lc3lang("oss") -> <<"Ossetic"/utf8>>;
lc3lang("oto") -> <<"Otomian languages"/utf8>>;
lc3lang("pal") -> <<"Pahlavi"/utf8>>;
lc3lang("pau") -> <<"Palauan"/utf8>>;
lc3lang("pli") -> <<"Pali"/utf8>>;
lc3lang("pam") -> <<"Pampanga"/utf8>>;
lc3lang("pag") -> <<"Pangasinan"/utf8>>;
lc3lang("pan") -> <<"Panjabi"/utf8>>;
lc3lang("pap") -> <<"Papiamento"/utf8>>;
lc3lang("paa") -> <<"Papuan-Australian (Other)"/utf8>>;
lc3lang("fas") -> <<"Persian"/utf8>>;
lc3lang("peo") -> <<"Persian, Old (ca 600 - 400 B.C.)"/utf8>>;
lc3lang("phn") -> <<"Phoenician"/utf8>>;
lc3lang("pol") -> <<"Polish"/utf8>>;
lc3lang("pon") -> <<"Ponape"/utf8>>;
lc3lang("por") -> <<"Portuguese"/utf8>>;
lc3lang("pra") -> <<"Prakrit languages"/utf8>>;
lc3lang("pro") -> <<"Provencal, Old (to 1500)"/utf8>>;
lc3lang("pus") -> <<"Pushto"/utf8>>;
lc3lang("que") -> <<"Quechua"/utf8>>;
lc3lang("roh") -> <<"Rhaeto-Romance"/utf8>>;
lc3lang("raj") -> <<"Rajasthani"/utf8>>;
lc3lang("rar") -> <<"Rarotongan"/utf8>>;
lc3lang("roa") -> <<"Romance (Other)"/utf8>>;
lc3lang("ron") -> <<"Romanian"/utf8>>;
lc3lang("rom") -> <<"Romany"/utf8>>;
lc3lang("run") -> <<"Rundi"/utf8>>;
lc3lang("rus") -> <<"Russian"/utf8>>;
lc3lang("sal") -> <<"Salishan languages"/utf8>>;
lc3lang("sam") -> <<"Samaritan Aramaic"/utf8>>;
lc3lang("smi") -> <<"Sami languages"/utf8>>;
lc3lang("smo") -> <<"Samoan"/utf8>>;
lc3lang("sad") -> <<"Sandawe"/utf8>>;
lc3lang("sag") -> <<"Sango"/utf8>>;
lc3lang("san") -> <<"Sanskrit"/utf8>>;
lc3lang("srd") -> <<"Sardinian"/utf8>>;
lc3lang("sco") -> <<"Scots"/utf8>>;
lc3lang("sel") -> <<"Selkup"/utf8>>;
lc3lang("sem") -> <<"Semitic (Other)"/utf8>>;
lc3lang("sr")  -> <<"Serbian"/utf8>>;
lc3lang("scr") -> <<"Serbo-Croatian"/utf8>>;
lc3lang("srr") -> <<"Serer"/utf8>>;
lc3lang("shn") -> <<"Shan"/utf8>>;
lc3lang("sna") -> <<"Shona"/utf8>>;
lc3lang("sid") -> <<"Sidamo"/utf8>>;
lc3lang("bla") -> <<"Siksika"/utf8>>;
lc3lang("snd") -> <<"Sindhi"/utf8>>;
lc3lang("sin") -> <<"Singhalese"/utf8>>;
lc3lang("sit") -> <<"Sino-Tibetan (Other)"/utf8>>;
lc3lang("sio") -> <<"Siouan languages"/utf8>>;
lc3lang("sla") -> <<"Slavic (Other)"/utf8>>;
lc3lang("ssw") -> <<"Siswant"/utf8>>;
lc3lang("slk") -> <<"Slovak"/utf8>>;
lc3lang("slv") -> <<"Slovenian"/utf8>>;
lc3lang("sog") -> <<"Sogdian"/utf8>>;
lc3lang("som") -> <<"Somali"/utf8>>;
lc3lang("son") -> <<"Songhai"/utf8>>;
lc3lang("wen") -> <<"Sorbian languages"/utf8>>;
lc3lang("nso") -> <<"Sotho, Northern"/utf8>>;
lc3lang("sot") -> <<"Sotho, Southern"/utf8>>;
lc3lang("sai") -> <<"South American Indian (Other)"/utf8>>;
lc3lang("esl") -> <<"Spanish"/utf8>>;
lc3lang("suk") -> <<"Sukuma"/utf8>>;
lc3lang("sux") -> <<"Sumerian"/utf8>>;
lc3lang("sun") -> <<"Sudanese"/utf8>>;
lc3lang("sus") -> <<"Susu"/utf8>>;
lc3lang("swa") -> <<"Swahili"/utf8>>;
%%% ??? same as 'Siswant' lc3lang("ssw") -> <<"Swazi"/utf8>>;
lc3lang("sve") -> <<"Swedish"/utf8>>;
lc3lang("syr") -> <<"Syriac"/utf8>>;
lc3lang("tgl") -> <<"Tagalog"/utf8>>;
lc3lang("tah") -> <<"Tahitian"/utf8>>;
lc3lang("tgk") -> <<"Tajik"/utf8>>;
lc3lang("tmh") -> <<"Tamashek"/utf8>>;
lc3lang("tam") -> <<"Tamil"/utf8>>;
lc3lang("tat") -> <<"Tatar"/utf8>>;
lc3lang("tel") -> <<"Telugu"/utf8>>;
lc3lang("ter") -> <<"Tereno"/utf8>>;
lc3lang("tha") -> <<"Thai"/utf8>>;
lc3lang("bod") -> <<"Tibetan"/utf8>>;
lc3lang("tig") -> <<"Tigre"/utf8>>;
lc3lang("tir") -> <<"Tigrinya"/utf8>>;
lc3lang("tem") -> <<"Timne"/utf8>>;
lc3lang("tiv") -> <<"Tivi"/utf8>>;
lc3lang("tli") -> <<"Tlingit"/utf8>>;
lc3lang("tog") -> <<"Tonga (Nyasa)"/utf8>>;
lc3lang("ton") -> <<"Tonga (Tonga Islands)"/utf8>>;
lc3lang("tru") -> <<"Truk"/utf8>>;
lc3lang("tsi") -> <<"Tsimshian"/utf8>>;
lc3lang("tso") -> <<"Tsonga"/utf8>>;
lc3lang("tsn") -> <<"Tswana"/utf8>>;
lc3lang("tum") -> <<"Tumbuka"/utf8>>;
lc3lang("tur") -> <<"Turkish"/utf8>>;
lc3lang("ota") -> <<"Turkish, Ottoman (1500 - 1928)"/utf8>>;
lc3lang("tuk") -> <<"Turkmen"/utf8>>;
lc3lang("tyv") -> <<"Tuvinian"/utf8>>;
lc3lang("twi") -> <<"Twi"/utf8>>;
lc3lang("uga") -> <<"Ugaritic"/utf8>>;
lc3lang("uig") -> <<"Uighur"/utf8>>;
lc3lang("ukr") -> <<"Ukrainian"/utf8>>;
lc3lang("umb") -> <<"Umbundu"/utf8>>;
lc3lang("und") -> <<"Undetermined"/utf8>>;
lc3lang("urd") -> <<"Urdu"/utf8>>;
lc3lang("uzb") -> <<"Uzbek"/utf8>>;
lc3lang("vai") -> <<"Vai"/utf8>>;
lc3lang("ven") -> <<"Venda"/utf8>>;
lc3lang("vie") -> <<"Vietnamese"/utf8>>;
lc3lang("vol") -> <<"Volapük"/utf8>>;
lc3lang("vot") -> <<"Votic"/utf8>>;
lc3lang("wak") -> <<"Wakashan languages"/utf8>>;
lc3lang("wal") -> <<"Walamo"/utf8>>;
lc3lang("war") -> <<"Waray"/utf8>>;
lc3lang("was") -> <<"Washo"/utf8>>;
lc3lang("cym") -> <<"Welsh"/utf8>>;
lc3lang("wol") -> <<"Wolof"/utf8>>;
lc3lang("xho") -> <<"Xhosa"/utf8>>;
lc3lang("sah") -> <<"Yakut"/utf8>>;
lc3lang("yao") -> <<"Yao"/utf8>>;
lc3lang("yap") -> <<"Yap"/utf8>>;
lc3lang("yid") -> <<"Yiddish"/utf8>>;
lc3lang("yor") -> <<"Yoruba"/utf8>>;
lc3lang("zap") -> <<"Zapotec"/utf8>>;
lc3lang("zen") -> <<"Zenaga"/utf8>>;
lc3lang("zha") -> <<"Zhuang"/utf8>>;
lc3lang("zul") -> <<"Zulu"/utf8>>;
lc3lang("zun") -> <<"Zuni"/utf8>>;
lc3lang(_)     -> <<""/utf8>>.


all3lang() ->
    [{"abk", <<"Abkhazian"/utf8>>},
     {"ace", <<"Achinese"/utf8>>},
     {"ach", <<"Acoli"/utf8>>},
     {"ada", <<"Adangme"/utf8>>},
     {"aar", <<"Afar"/utf8>>},
     {"afh", <<"Afrihili"/utf8>>},
     {"afr", <<"Afrikaans"/utf8>>},
     {"afa", <<"Afro-Asiatic (Other)"/utf8>>},
     {"aka", <<"Akan"/utf8>>},
     {"akk", <<"Akkadian"/utf8>>},
     {"alb", <<"Albanian"/utf8>>},
     {"ale", <<"Aleut"/utf8>>},
     {"alg", <<"Algonquian languages"/utf8>>},
     {"tut", <<"Altaic (Other)"/utf8>>},
     {"amh", <<"Amharic"/utf8>>},
     {"apa", <<"Apache languages"/utf8>>},
     {"ara", <<"Arabic"/utf8>>},
     {"arc", <<"Aramaic"/utf8>>},
     {"arp", <<"Arapaho"/utf8>>},
     {"arn", <<"Araucanian"/utf8>>},
     {"arw", <<"Arawak"/utf8>>},
     {"arm", <<"Armenian"/utf8>>},
     {"art", <<"Artificial (Other)"/utf8>>},
     {"asm", <<"Assamese"/utf8>>},
     {"ath", <<"Athapascan languages"/utf8>>},
     {"map", <<"Austronesian (Other)"/utf8>>},
     {"ava", <<"Avaric"/utf8>>},
     {"ave", <<"Avestan"/utf8>>},
     {"awa", <<"Awadhi"/utf8>>},
     {"aym", <<"Aymara"/utf8>>},
     {"aze", <<"Azerbaijani"/utf8>>},
     {"nah", <<"Aztec"/utf8>>},
     {"ban", <<"Balinese"/utf8>>},
     {"bat", <<"Baltic (Other)"/utf8>>},
     {"bal", <<"Baluchi"/utf8>>},
     {"bam", <<"Bambara"/utf8>>},
     {"bai", <<"Bamileke languages"/utf8>>},
     {"bad", <<"Banda"/utf8>>},
     {"bnt", <<"Bantu (Other)"/utf8>>},
     {"bas", <<"Basa"/utf8>>},
     {"bak", <<"Bashkir"/utf8>>},
     {"baq", <<"Basque"/utf8>>},
     {"bej", <<"Beja"/utf8>>},
     {"bem", <<"Bemba"/utf8>>},
     {"ben", <<"Bengali"/utf8>>},
     {"ber", <<"Berber (Other)"/utf8>>},
     {"bho", <<"Bhojpuri"/utf8>>},
     {"bih", <<"Bihari"/utf8>>},
     {"bik", <<"Bikol"/utf8>>},
     {"bin", <<"Bini"/utf8>>},
     {"bis", <<"Bislama"/utf8>>},
     {"bra", <<"Braj"/utf8>>},
     {"bre", <<"Breton"/utf8>>},
     {"bug", <<"Buginese"/utf8>>},
     {"bul", <<"Bulgarian"/utf8>>},
     {"bua", <<"Buriat"/utf8>>},
     {"bur", <<"Burmese"/utf8>>},
     {"bel", <<"Byelorussian"/utf8>>},
     {"cad", <<"Caddo"/utf8>>},
     {"car", <<"Carib"/utf8>>},
     {"cat", <<"Catalan"/utf8>>},
     {"cau", <<"Caucasian (Other)"/utf8>>},
     {"ceb", <<"Cebuano"/utf8>>},
     {"cel", <<"Celtic (Other)"/utf8>>},
     {"cai", <<"Central American Indian (Other)"/utf8>>},
     {"chg", <<"Chagatai"/utf8>>},
     {"cha", <<"Chamorro"/utf8>>},
     {"che", <<"Chechen"/utf8>>},
     {"chr", <<"Cherokee"/utf8>>},
     {"chy", <<"Cheyenne"/utf8>>},
     {"chb", <<"Chibcha"/utf8>>},
     {"chi", <<"Chinese"/utf8>>},
     {"chn", <<"Chinook jargon"/utf8>>},
     {"cho", <<"Choctaw"/utf8>>},
     {"chu", <<"Church Slavic"/utf8>>},
     {"chv", <<"Chuvash"/utf8>>},
     {"cop", <<"Coptic"/utf8>>},
     {"cor", <<"Cornish"/utf8>>},
     {"cos", <<"Corsican"/utf8>>},
     {"cre", <<"Cree"/utf8>>},
     {"mus", <<"Creek"/utf8>>},
     {"crp", <<"Creoles and Pidgins (Other)"/utf8>>},
     {"cpe", <<"Creoles and Pidgins, English-based (Other)"/utf8>>},
     {"cpf", <<"Creoles and Pidgins, French-based (Other)"/utf8>>},
     {"cpp", <<"Creoles and Pidgins, Portuguese-based (Other)"/utf8>>},
     {"cus", <<"Cushitic (Other)"/utf8>>},
     {"hr", <<"Croatian"/utf8>>},
     {"ces", <<"Czech"/utf8>>},
     {"dak", <<"Dakota"/utf8>>},
     {"dan", <<"Danish"/utf8>>},
     {"del", <<"Delaware"/utf8>>},
     {"din", <<"Dinka"/utf8>>},
     {"div", <<"Divehi"/utf8>>},
     {"doi", <<"Dogri"/utf8>>},
     {"dra", <<"Dravidian (Other)"/utf8>>},
     {"dua", <<"Duala"/utf8>>},
     {"dut", <<"Dutch"/utf8>>},
     {"dum", <<"Dutch, Middle (ca. 1050-1350)"/utf8>>},
     {"dyu", <<"Dyula"/utf8>>},
     {"dzo", <<"Dzongkha"/utf8>>},
     {"efi", <<"Efik"/utf8>>},
     {"egy", <<"Egyptian (Ancient)"/utf8>>},
     {"eka", <<"Ekajuk"/utf8>>},
     {"elx", <<"Elamite"/utf8>>},
     {"eng", <<"English"/utf8>>},
     {"enm", <<"English, Middle (ca. 1100-1500)"/utf8>>},
     {"ang", <<"English, Old (ca. 450-1100)"/utf8>>},
     {"esk", <<"Eskimo (Other)"/utf8>>},
     {"epo", <<"Esperanto"/utf8>>},
     {"est", <<"Estonian"/utf8>>},
     {"ewe", <<"Ewe"/utf8>>},
     {"ewo", <<"Ewondo"/utf8>>},
     {"fan", <<"Fang"/utf8>>},
     {"fat", <<"Fanti"/utf8>>},
     {"fao", <<"Faroese"/utf8>>},
     {"fij", <<"Fijian"/utf8>>},
     {"fin", <<"Finnish"/utf8>>},
     {"fiu", <<"Finno-Ugrian (Other)"/utf8>>},
     {"fon", <<"Fon"/utf8>>},
     {"fra", <<"French"/utf8>>},
     {"frm", <<"French, Middle (ca. 1400-1600)"/utf8>>},
     {"fro", <<"French, Old (842- ca. 1400)"/utf8>>},
     {"fry", <<"Frisian"/utf8>>},
     {"ful", <<"Fulah"/utf8>>},
     {"gaa", <<"Ga"/utf8>>},
     {"gae", <<" (Scots)"/utf8>>},
     {"glg", <<"Gallegan"/utf8>>},
     {"lug", <<"Ganda"/utf8>>},
     {"gay", <<"Gayo"/utf8>>},
     {"gez", <<"Geez"/utf8>>},
     {"geo", <<"Georgian"/utf8>>},
     {"deu", <<"German"/utf8>>},
     {"gmh", <<"German, Middle High (ca. 1050-1500)"/utf8>>},
     {"goh", <<"German, Old High (ca. 750-1050)"/utf8>>},
     {"gem", <<"Germanic (Other)"/utf8>>},
     {"gil", <<"Gilbertese"/utf8>>},
     {"gon", <<"Gondi"/utf8>>},
     {"got", <<"Gothic"/utf8>>},
     {"grb", <<"Grebo"/utf8>>},
     {"grc", <<"Greek, Ancient (to 1453)"/utf8>>},
     {"ell", <<"Greek, Modern (1453-)"/utf8>>},
     {"kal", <<"Greenlandic"/utf8>>},
     {"grn", <<"Guarani"/utf8>>},
     {"guj", <<"Gujarati"/utf8>>},
     {"hai", <<"Haida"/utf8>>},
     {"hau", <<"Hausa"/utf8>>},
     {"haw", <<"Hawaiian"/utf8>>},
     {"heb", <<"Hebrew"/utf8>>},
     {"her", <<"Herero"/utf8>>},
     {"hil", <<"Hiligaynon"/utf8>>},
     {"him", <<"Himachali"/utf8>>},
     {"hin", <<"Hindi"/utf8>>},
     {"hmo", <<"Hiri Motu"/utf8>>},
     {"hun", <<"Hungarian"/utf8>>},
     {"hup", <<"Hupa"/utf8>>},
     {"iba", <<"Iban"/utf8>>},
     {"ice", <<"Icelandic"/utf8>>},
     {"ibo", <<"Igbo"/utf8>>},
     {"ijo", <<"Ijo"/utf8>>},
     {"ilo", <<"Iloko"/utf8>>},
     {"inc", <<"Indic (Other)"/utf8>>},
     {"ine", <<"Indo-European (Other)"/utf8>>},
     {"ind", <<"Indonesian"/utf8>>},
     {"ina", <<"Interlingua (International Auxiliary language Association)"/utf8>>},
     {"ine", <<"Interlingue"/utf8>>},
     {"iku", <<"Inuktitut"/utf8>>},
     {"ipk", <<"Inupiak"/utf8>>},
     {"ira", <<"Iranian (Other)"/utf8>>},
     {"gai", <<"Irish"/utf8>>},
     {"sga", <<"Irish, Old (to 900)"/utf8>>},
     {"mga", <<"Irish, Middle (900 - 1200)"/utf8>>},
     {"iro", <<"Iroquoian languages"/utf8>>},
     {"ita", <<"Italian"/utf8>>},
     {"jpn", <<"Japanese"/utf8>>},
     {"jav", <<"Javanese"/utf8>>},
     {"jrb", <<"Judeo-Arabic"/utf8>>},
     {"jpr", <<"Judeo-Persian"/utf8>>},
     {"kab", <<"Kabyle"/utf8>>},
     {"kac", <<"Kachin"/utf8>>},
     {"kam", <<"Kamba"/utf8>>},
     {"kan", <<"Kannada"/utf8>>},
     {"kau", <<"Kanuri"/utf8>>},
     {"kaa", <<"Kara-Kalpak"/utf8>>},
     {"kar", <<"Karen"/utf8>>},
     {"kas", <<"Kashmiri"/utf8>>},
     {"kaw", <<"Kawi"/utf8>>},
     {"kaz", <<"Kazakh"/utf8>>},
     {"kha", <<"Khasi"/utf8>>},
     {"khm", <<"Khmer"/utf8>>},
     {"khi", <<"Khoisan (Other)"/utf8>>},
     {"kho", <<"Khotanese"/utf8>>},
     {"kik", <<"Kikuyu"/utf8>>},
     {"kin", <<"Kinyarwanda"/utf8>>},
     {"kir", <<"Kirghiz"/utf8>>},
     {"kom", <<"Komi"/utf8>>},
     {"kon", <<"Kongo"/utf8>>},
     {"kok", <<"Konkani"/utf8>>},
     {"kor", <<"Korean"/utf8>>},
     {"kpe", <<"Kpelle"/utf8>>},
     {"kro", <<"Kru"/utf8>>},
     {"kua", <<"Kuanyama"/utf8>>},
     {"kum", <<"Kumyk"/utf8>>},
     {"kur", <<"Kurdish"/utf8>>},
     {"kru", <<"Kurukh"/utf8>>},
     {"kus", <<"Kusaie"/utf8>>},
     {"kut", <<"Kutenai"/utf8>>},
     {"lad", <<"Ladino"/utf8>>},
     {"lah", <<"Lahnda"/utf8>>},
     {"lam", <<"Lamba"/utf8>>},
     {"oci", <<"Langue d'Oc (post 1500)"/utf8>>},
     {"lao", <<"Lao"/utf8>>},
     {"lat", <<"Latin"/utf8>>},
     {"lav", <<"Latvian"/utf8>>},
     {"ltz", <<"Letzeburgesch"/utf8>>},
     {"lez", <<"Lezghian"/utf8>>},
     {"lin", <<"Lingala"/utf8>>},
     {"lit", <<"Lithuanian"/utf8>>},
     {"loz", <<"Lozi"/utf8>>},
     {"lub", <<"Luba-Katanga"/utf8>>},
     {"lui", <<"Luiseno"/utf8>>},
     {"lun", <<"Lunda"/utf8>>},
     {"luo", <<"Luo (Kenya and Tanzania)"/utf8>>},
     {"mac", <<"Macedonian"/utf8>>},
     {"mad", <<"Madurese"/utf8>>},
     {"mag", <<"Magahi"/utf8>>},
     {"mai", <<"Maithili"/utf8>>},
     {"mak", <<"Makasar"/utf8>>},
     {"mlg", <<"Malagasy"/utf8>>},
     {"may", <<"Malay"/utf8>>},
     {"mal", <<"Malayalam"/utf8>>},
     {"mlt", <<"Maltese"/utf8>>},
     {"man", <<"Mandingo"/utf8>>},
     {"mni", <<"Manipuri"/utf8>>},
     {"mno", <<"Manobo languages"/utf8>>},
     {"max", <<"Manx"/utf8>>},
     {"mao", <<"Maori"/utf8>>},
     {"mar", <<"Marathi"/utf8>>},
     {"chm", <<"Mari"/utf8>>},
     {"mah", <<"Marshall"/utf8>>},
     {"mwr", <<"Marwari"/utf8>>},
     {"mas", <<"Masai"/utf8>>},
     {"myn", <<"Mayan languages"/utf8>>},
     {"men", <<"Mende"/utf8>>},
     {"mic", <<"Micmac"/utf8>>},
     {"min", <<"Minangkabau"/utf8>>},
     {"mis", <<"Miscellaneous (Other)"/utf8>>},
     {"moh", <<"Mohawk"/utf8>>},
     {"mol", <<"Moldavian"/utf8>>},
     {"mkh", <<"Mon-Kmer (Other)"/utf8>>},
     {"lol", <<"Mongo"/utf8>>},
     {"mon", <<"Mongolian"/utf8>>},
     {"mos", <<"Mossi"/utf8>>},
     {"mul", <<"Multiple languages"/utf8>>},
     {"mun", <<"Munda languages"/utf8>>},
     {"nau", <<"Nauru"/utf8>>},
     {"nav", <<"Navajo"/utf8>>},
     {"nde", <<"Ndebele, North"/utf8>>},
     {"nbl", <<"Ndebele, South"/utf8>>},
     {"ndo", <<"Ndongo"/utf8>>},
     {"nep", <<"Nepali"/utf8>>},
     {"new", <<"Newari"/utf8>>},
     {"nic", <<"Niger-Kordofanian (Other)"/utf8>>},
     {"ssa", <<"Nilo-Saharan (Other)"/utf8>>},
     {"niu", <<"Niuean"/utf8>>},
     {"non", <<"Norse, Old"/utf8>>},
     {"nai", <<"North American Indian (Other)"/utf8>>},
     {"nor", <<"Norwegian"/utf8>>},
     {"nno", <<"Norwegian (Nynorsk)"/utf8>>},
     {"nub", <<"Nubian languages"/utf8>>},
     {"nym", <<"Nyamwezi"/utf8>>},
     {"nya", <<"Nyanja"/utf8>>},
     {"nyn", <<"Nyankole"/utf8>>},
     {"nyo", <<"Nyoro"/utf8>>},
     {"nzi", <<"Nzima"/utf8>>},
     {"oji", <<"Ojibwa"/utf8>>},
     {"ori", <<"Oriya"/utf8>>},
     {"orm", <<"Oromo"/utf8>>},
     {"osa", <<"Osage"/utf8>>},
     {"oss", <<"Ossetic"/utf8>>},
     {"oto", <<"Otomian languages"/utf8>>},
     {"pal", <<"Pahlavi"/utf8>>},
     {"pau", <<"Palauan"/utf8>>},
     {"pli", <<"Pali"/utf8>>},
     {"pam", <<"Pampanga"/utf8>>},
     {"pag", <<"Pangasinan"/utf8>>},
     {"pan", <<"Panjabi"/utf8>>},
     {"pap", <<"Papiamento"/utf8>>},
     {"paa", <<"Papuan-Australian (Other)"/utf8>>},
     {"fas", <<"Persian"/utf8>>},
     {"peo", <<"Persian, Old (ca 600 - 400 B.C.)"/utf8>>},
     {"phn", <<"Phoenician"/utf8>>},
     {"pol", <<"Polish"/utf8>>},
     {"pon", <<"Ponape"/utf8>>},
     {"por", <<"Portuguese"/utf8>>},
     {"pra", <<"Prakrit languages"/utf8>>},
     {"pro", <<"Provencal, Old (to 1500)"/utf8>>},
     {"pus", <<"Pushto"/utf8>>},
     {"que", <<"Quechua"/utf8>>},
     {"roh", <<"Rhaeto-Romance"/utf8>>},
     {"raj", <<"Rajasthani"/utf8>>},
     {"rar", <<"Rarotongan"/utf8>>},
     {"roa", <<"Romance (Other)"/utf8>>},
     {"ron", <<"Romanian"/utf8>>},
     {"rom", <<"Romany"/utf8>>},
     {"run", <<"Rundi"/utf8>>},
     {"rus", <<"Russian"/utf8>>},
     {"sal", <<"Salishan languages"/utf8>>},
     {"sam", <<"Samaritan Aramaic"/utf8>>},
     {"smi", <<"Sami languages"/utf8>>},
     {"smo", <<"Samoan"/utf8>>},
     {"sad", <<"Sandawe"/utf8>>},
     {"sag", <<"Sango"/utf8>>},
     {"san", <<"Sanskrit"/utf8>>},
     {"srd", <<"Sardinian"/utf8>>},
     {"sco", <<"Scots"/utf8>>},
     {"sel", <<"Selkup"/utf8>>},
     {"sem", <<"Semitic (Other)"/utf8>>},
     {"sr", <<"Serbian"/utf8>>},
     {"scr", <<"Serbo-Croatian"/utf8>>},
     {"srr", <<"Serer"/utf8>>},
     {"shn", <<"Shan"/utf8>>},
     {"sna", <<"Shona"/utf8>>},
     {"sid", <<"Sidamo"/utf8>>},
     {"bla", <<"Siksika"/utf8>>},
     {"snd", <<"Sindhi"/utf8>>},
     {"sin", <<"Singhalese"/utf8>>},
     {"sit", <<"Sino-Tibetan (Other)"/utf8>>},
     {"sio", <<"Siouan languages"/utf8>>},
     {"sla", <<"Slavic (Other)"/utf8>>},
     {"ssw", <<"Siswant"/utf8>>},
     {"slk", <<"Slovak"/utf8>>},
     {"slv", <<"Slovenian"/utf8>>},
     {"sog", <<"Sogdian"/utf8>>},
     {"som", <<"Somali"/utf8>>},
     {"son", <<"Songhai"/utf8>>},
     {"wen", <<"Sorbian languages"/utf8>>},
     {"nso", <<"Sotho, Northern"/utf8>>},
     {"sot", <<"Sotho, Southern"/utf8>>},
     {"sai", <<"South American Indian (Other)"/utf8>>},
     {"esl", <<"Spanish"/utf8>>},
     {"suk", <<"Sukuma"/utf8>>},
     {"sux", <<"Sumerian"/utf8>>},
     {"sun", <<"Sudanese"/utf8>>},
     {"sus", <<"Susu"/utf8>>},
     {"swa", <<"Swahili"/utf8>>},
     {"ssw", <<"Swazi"/utf8>>},
     {"sve", <<"Swedish"/utf8>>},
     {"syr", <<"Syriac"/utf8>>},
     {"tgl", <<"Tagalog"/utf8>>},
     {"tah", <<"Tahitian"/utf8>>},
     {"tgk", <<"Tajik"/utf8>>},
     {"tmh", <<"Tamashek"/utf8>>},
     {"tam", <<"Tamil"/utf8>>},
     {"tat", <<"Tatar"/utf8>>},
     {"tel", <<"Telugu"/utf8>>},
     {"ter", <<"Tereno"/utf8>>},
     {"tha", <<"Thai"/utf8>>},
     {"bod", <<"Tibetan"/utf8>>},
     {"tig", <<"Tigre"/utf8>>},
     {"tir", <<"Tigrinya"/utf8>>},
     {"tem", <<"Timne"/utf8>>},
     {"tiv", <<"Tivi"/utf8>>},
     {"tli", <<"Tlingit"/utf8>>},
     {"tog", <<"Tonga (Nyasa)"/utf8>>},
     {"ton", <<"Tonga (Tonga Islands)"/utf8>>},
     {"tru", <<"Truk"/utf8>>},
     {"tsi", <<"Tsimshian"/utf8>>},
     {"tso", <<"Tsonga"/utf8>>},
     {"tsn", <<"Tswana"/utf8>>},
     {"tum", <<"Tumbuka"/utf8>>},
     {"tur", <<"Turkish"/utf8>>},
     {"ota", <<"Turkish, Ottoman (1500 - 1928)"/utf8>>},
     {"tuk", <<"Turkmen"/utf8>>},
     {"tyv", <<"Tuvinian"/utf8>>},
     {"twi", <<"Twi"/utf8>>},
     {"uga", <<"Ugaritic"/utf8>>},
     {"uig", <<"Uighur"/utf8>>},
     {"ukr", <<"Ukrainian"/utf8>>},
     {"umb", <<"Umbundu"/utf8>>},
     {"und", <<"Undetermined"/utf8>>},
     {"urd", <<"Urdu"/utf8>>},
     {"uzb", <<"Uzbek"/utf8>>},
     {"vai", <<"Vai"/utf8>>},
     {"ven", <<"Venda"/utf8>>},
     {"vie", <<"Vietnamese"/utf8>>},
     {"vol", <<"Volapük"/utf8>>},
     {"vot", <<"Votic"/utf8>>},
     {"wak", <<"Wakashan languages"/utf8>>},
     {"wal", <<"Walamo"/utf8>>},
     {"war", <<"Waray"/utf8>>},
     {"was", <<"Washo"/utf8>>},
     {"cym", <<"Welsh"/utf8>>},
     {"wol", <<"Wolof"/utf8>>},
     {"xho", <<"Xhosa"/utf8>>},
     {"sah", <<"Yakut"/utf8>>},
     {"yao", <<"Yao"/utf8>>},
     {"yap", <<"Yap"/utf8>>},
     {"yid", <<"Yiddish"/utf8>>},
     {"yor", <<"Yoruba"/utf8>>},
     {"zap", <<"Zapotec"/utf8>>},
     {"zen", <<"Zenaga"/utf8>>},
     {"zha", <<"Zhuang"/utf8>>},
     {"zul", <<"Zulu"/utf8>>},
     {"zun", <<"Zuni"/utf8>>}
].

lc2lang("aa") -> <<"Afar"/utf8>>;
lc2lang("ab") -> <<"Abkhazian"/utf8>>;
lc2lang("ae") -> <<"Avestan"/utf8>>;
lc2lang("af") -> <<"Afrikaans"/utf8>>;
lc2lang("am") -> <<"Amharic"/utf8>>;
lc2lang("ar") -> <<"Arabic"/utf8>>;
lc2lang("as") -> <<"Assamese"/utf8>>;
lc2lang("ay") -> <<"Aymara"/utf8>>;
lc2lang("az") -> <<"Azerbaijani"/utf8>>;
lc2lang("ba") -> <<"Bashkir"/utf8>>;
lc2lang("be") -> <<"Byelorussian; Belarusian"/utf8>>;
lc2lang("bg") -> <<"Bulgarian"/utf8>>;
lc2lang("bh") -> <<"Bihari"/utf8>>;
lc2lang("bi") -> <<"Bislama"/utf8>>;
lc2lang("bn") -> <<"Bengali; Bangla"/utf8>>;
lc2lang("bo") -> <<"Tibetan"/utf8>>;
lc2lang("br") -> <<"Breton"/utf8>>;
lc2lang("bs") -> <<"Bosnian"/utf8>>;
lc2lang("ca") -> <<"Catalan"/utf8>>;
lc2lang("ce") -> <<"Chechen"/utf8>>;
lc2lang("ch") -> <<"Chamorro"/utf8>>;
lc2lang("co") -> <<"Corsican"/utf8>>;
lc2lang("cs") -> <<"Czech"/utf8>>;
lc2lang("cu") -> <<"Church Slavic"/utf8>>;
lc2lang("cv") -> <<"Chuvash"/utf8>>;
lc2lang("cy") -> <<"Welsh"/utf8>>;
lc2lang("da") -> <<"Danish"/utf8>>;
lc2lang("de") -> <<"German"/utf8>>;
lc2lang("dz") -> <<"Dzongkha; Bhutani"/utf8>>;
lc2lang("el") -> <<"Greek"/utf8>>;
lc2lang("en") -> <<"English"/utf8>>;
lc2lang("eo") -> <<"Esperanto"/utf8>>;
lc2lang("es") -> <<"Spanish"/utf8>>;
lc2lang("et") -> <<"Estonian"/utf8>>;
lc2lang("eu") -> <<"Basque"/utf8>>;
lc2lang("fa") -> <<"Persian"/utf8>>;
lc2lang("fi") -> <<"Finnish"/utf8>>;
lc2lang("fj") -> <<"Fijian; Fiji"/utf8>>;
lc2lang("fo") -> <<"Faroese"/utf8>>;
lc2lang("fr") -> <<"French"/utf8>>;
lc2lang("fy") -> <<"Frisian"/utf8>>;
lc2lang("ga") -> <<"Irish"/utf8>>;
lc2lang("gd") -> <<"Scots; Gaelic"/utf8>>;
lc2lang("gl") -> <<"Gallegan; Galician"/utf8>>;
lc2lang("gn") -> <<"Guarani"/utf8>>;
lc2lang("gu") -> <<"Gujarati"/utf8>>;
lc2lang("gv") -> <<"Manx"/utf8>>;
lc2lang("ha") -> <<"Hausa"/utf8>>;
lc2lang("he") -> <<"Hebrew"/utf8>>;
lc2lang("hi") -> <<"Hindi"/utf8>>;
lc2lang("ho") -> <<"Hiri Motu"/utf8>>;
lc2lang("hr") -> <<"Croatian"/utf8>>;
lc2lang("hu") -> <<"Hungarian"/utf8>>;
lc2lang("hy") -> <<"Armenian"/utf8>>;
lc2lang("hz") -> <<"Herero"/utf8>>;
lc2lang("ia") -> <<"Interlingua"/utf8>>;
lc2lang("id") -> <<"Indonesian"/utf8>>;
lc2lang("ie") -> <<"Interlingue"/utf8>>;
lc2lang("ik") -> <<"Inupiak"/utf8>>;
lc2lang("io") -> <<"Ido"/utf8>>;
lc2lang("is") -> <<"Icelandic"/utf8>>;
lc2lang("it") -> <<"Italian"/utf8>>;
lc2lang("iu") -> <<"Inuktitut"/utf8>>;
lc2lang("ja") -> <<"Japanese"/utf8>>;
lc2lang("jv") -> <<"Javanese"/utf8>>;
lc2lang("ka") -> <<"Georgian"/utf8>>;
lc2lang("ki") -> <<"Kikuyu"/utf8>>;
lc2lang("kj") -> <<"Kuanyama"/utf8>>;
lc2lang("kk") -> <<"Kazakh"/utf8>>;
lc2lang("kl") -> <<"Kalaallisut; Greenlandic"/utf8>>;
lc2lang("km") -> <<"Khmer; Cambodian"/utf8>>;
lc2lang("kn") -> <<"Kannada"/utf8>>;
lc2lang("ko") -> <<"Korean"/utf8>>;
lc2lang("ks") -> <<"Kashmiri"/utf8>>;
lc2lang("ku") -> <<"Kurdish"/utf8>>;
lc2lang("kv") -> <<"Komi"/utf8>>;
lc2lang("kw") -> <<"Cornish"/utf8>>;
lc2lang("ky") -> <<"Kirghiz"/utf8>>;
lc2lang("la") -> <<"Latin"/utf8>>;
lc2lang("lb") -> <<"Letzeburgesch"/utf8>>;
lc2lang("ln") -> <<"Lingala"/utf8>>;
lc2lang("lo") -> <<"Lao; Laotian"/utf8>>;
lc2lang("lt") -> <<"Lithuanian"/utf8>>;
lc2lang("lv") -> <<"Latvian; Lettish"/utf8>>;
lc2lang("mg") -> <<"Malagasy"/utf8>>;
lc2lang("mh") -> <<"Marshall"/utf8>>;
lc2lang("mi") -> <<"Maori"/utf8>>;
lc2lang("mk") -> <<"Macedonian"/utf8>>;
lc2lang("ml") -> <<"Malayalam"/utf8>>;
lc2lang("mn") -> <<"Mongolian"/utf8>>;
lc2lang("mo") -> <<"Moldavian"/utf8>>;
lc2lang("mr") -> <<"Marathi"/utf8>>;
lc2lang("ms") -> <<"Malay"/utf8>>;
lc2lang("mt") -> <<"Maltese"/utf8>>;
lc2lang("my") -> <<"Burmese"/utf8>>;
lc2lang("na") -> <<"Nauru"/utf8>>;
lc2lang("nb") -> <<"Norwegian Bokmål"/utf8>>;
lc2lang("nd") -> <<"Ndebele, North"/utf8>>;
lc2lang("ne") -> <<"Nepali"/utf8>>;
lc2lang("ng") -> <<"Ndonga"/utf8>>;
lc2lang("nl") -> <<"Dutch"/utf8>>;
lc2lang("nn") -> <<"Norwegian Nynorsk"/utf8>>;
lc2lang("no") -> <<"Norwegian"/utf8>>;
lc2lang("nr") -> <<"Ndebele, South"/utf8>>;
lc2lang("nv") -> <<"Navajo"/utf8>>;
lc2lang("ny") -> <<"Chichewa; Nyanja"/utf8>>;
lc2lang("oc") -> <<"Occitan; Provençal"/utf8>>;
lc2lang("om") -> <<"(Afan) Oromo"/utf8>>;
lc2lang("or") -> <<"Oriya"/utf8>>;
lc2lang("os") -> <<"Ossetian; Ossetic"/utf8>>;
lc2lang("pa") -> <<"Panjabi; Punjabi"/utf8>>;
lc2lang("pi") -> <<"Pali"/utf8>>;
lc2lang("pl") -> <<"Polish"/utf8>>;
lc2lang("ps") -> <<"Pashto, Pushto"/utf8>>;
lc2lang("pt") -> <<"Portuguese"/utf8>>;
lc2lang("qu") -> <<"Quechua"/utf8>>;
lc2lang("rm") -> <<"Rhaeto-Romance"/utf8>>;
lc2lang("rn") -> <<"Rundi; Kirundi"/utf8>>;
lc2lang("ro") -> <<"Romanian"/utf8>>;
lc2lang("ru") -> <<"Russian"/utf8>>;
lc2lang("rw") -> <<"Kinyarwanda"/utf8>>;
lc2lang("sa") -> <<"Sanskrit"/utf8>>;
lc2lang("sc") -> <<"Sardinian"/utf8>>;
lc2lang("sd") -> <<"Sindhi"/utf8>>;
lc2lang("se") -> <<"Northern Sami"/utf8>>;
lc2lang("sg") -> <<"Sango; Sangro"/utf8>>;
lc2lang("si") -> <<"Sinhalese"/utf8>>;
lc2lang("sk") -> <<"Slovak"/utf8>>;
lc2lang("sl") -> <<"Slovenian"/utf8>>;
lc2lang("sm") -> <<"Samoan"/utf8>>;
lc2lang("sn") -> <<"Shona"/utf8>>;
lc2lang("so") -> <<"Somali"/utf8>>;
lc2lang("sq") -> <<"Albanian"/utf8>>;
lc2lang("sr") -> <<"Serbian"/utf8>>;
lc2lang("ss") -> <<"Swati; Siswati"/utf8>>;
lc2lang("st") -> <<"Sesotho; Sotho, Southern"/utf8>>;
lc2lang("su") -> <<"Sundanese"/utf8>>;
lc2lang("sv") -> <<"Swedish"/utf8>>;
lc2lang("sw") -> <<"Swahili"/utf8>>;
lc2lang("ta") -> <<"Tamil"/utf8>>;
lc2lang("te") -> <<"Telugu"/utf8>>;
lc2lang("tg") -> <<"Tajik"/utf8>>;
lc2lang("th") -> <<"Thai"/utf8>>;
lc2lang("ti") -> <<"Tigrinya"/utf8>>;
lc2lang("tk") -> <<"Turkmen"/utf8>>;
lc2lang("tl") -> <<"Tagalog"/utf8>>;
lc2lang("tn") -> <<"Tswana; Setswana"/utf8>>;
lc2lang("to") -> <<"Tonga"/utf8>>;
lc2lang("tr") -> <<"Turkish"/utf8>>;
lc2lang("ts") -> <<"Tsonga"/utf8>>;
lc2lang("tt") -> <<"Tatar"/utf8>>;
lc2lang("tw") -> <<"Twi"/utf8>>;
lc2lang("ty") -> <<"Tahitian"/utf8>>;
lc2lang("ug") -> <<"Uighur"/utf8>>;
lc2lang("uk") -> <<"Ukrainian"/utf8>>;
lc2lang("ur") -> <<"Urdu"/utf8>>;
lc2lang("uz") -> <<"Uzbek"/utf8>>;
lc2lang("vi") -> <<"Vietnamese"/utf8>>;
lc2lang("vo") -> <<"Volapuk"/utf8>>;
lc2lang("wa") -> <<"Walloon"/utf8>>;
lc2lang("wo") -> <<"Wolof"/utf8>>;
lc2lang("xh") -> <<"Xhosa"/utf8>>;
lc2lang("yi") -> <<"Yiddish"/utf8>>;
lc2lang("yo") -> <<"Yoruba"/utf8>>;
lc2lang("za") -> <<"Zhuang"/utf8>>;
lc2lang("zh") -> <<"Chinese"/utf8>>;
lc2lang("zu") -> <<"Zulu"/utf8>>;
lc2lang(_)   -> <<""/utf8>>.

all2lang() ->
    [{"aa", <<"Afar"/utf8>>},
     {"ab", <<"Abkhazian"/utf8>>},
     {"ae", <<"Avestan"/utf8>>},
     {"af", <<"Afrikaans"/utf8>>},
     {"am", <<"Amharic"/utf8>>},
     {"ar", <<"Arabic"/utf8>>},
     {"as", <<"Assamese"/utf8>>},
     {"ay", <<"Aymara"/utf8>>},
     {"az", <<"Azerbaijani"/utf8>>},
     {"ba", <<"Bashkir"/utf8>>},
     {"be", <<"Byelorussian; Belarusian"/utf8>>},
     {"bg", <<"Bulgarian"/utf8>>},
     {"bh", <<"Bihari"/utf8>>},
     {"bi", <<"Bislama"/utf8>>},
     {"bn", <<"Bengali; Bangla"/utf8>>},
     {"bo", <<"Tibetan"/utf8>>},
     {"br", <<"Breton"/utf8>>},
     {"bs", <<"Bosnian"/utf8>>},
     {"ca", <<"Catalan"/utf8>>},
     {"ce", <<"Chechen"/utf8>>},
     {"ch", <<"Chamorro"/utf8>>},
     {"co", <<"Corsican"/utf8>>},
     {"cs", <<"Czech"/utf8>>},
     {"cu", <<"Church Slavic"/utf8>>},
     {"cv", <<"Chuvash"/utf8>>},
     {"cy", <<"Welsh"/utf8>>},
     {"da", <<"Danish"/utf8>>},
     {"de", <<"German"/utf8>>},
     {"dz", <<"Dzongkha; Bhutani"/utf8>>},
     {"el", <<"Greek"/utf8>>},
     {"en", <<"English"/utf8>>},
     {"eo", <<"Esperanto"/utf8>>},
     {"es", <<"Spanish"/utf8>>},
     {"et", <<"Estonian"/utf8>>},
     {"eu", <<"Basque"/utf8>>},
     {"fa", <<"Persian"/utf8>>},
     {"fi", <<"Finnish"/utf8>>},
     {"fj", <<"Fijian; Fiji"/utf8>>},
     {"fo", <<"Faroese"/utf8>>},
     {"fr", <<"French"/utf8>>},
     {"fy", <<"Frisian"/utf8>>},
     {"ga", <<"Irish"/utf8>>},
     {"gd", <<"Scots; Gaelic"/utf8>>},
     {"gl", <<"Gallegan; Galician"/utf8>>},
     {"gn", <<"Guarani"/utf8>>},
     {"gu", <<"Gujarati"/utf8>>},
     {"gv", <<"Manx"/utf8>>},
     {"ha", <<"Hausa"/utf8>>},
     {"he", <<"Hebrew"/utf8>>},
     {"hi", <<"Hindi"/utf8>>},
     {"ho", <<"Hiri Motu"/utf8>>},
     {"hr", <<"Croatian"/utf8>>},
     {"hu", <<"Hungarian"/utf8>>},
     {"hy", <<"Armenian"/utf8>>},
     {"hz", <<"Herero"/utf8>>},
     {"ia", <<"Interlingua"/utf8>>},
     {"id", <<"Indonesian"/utf8>>},
     {"ie", <<"Interlingue"/utf8>>},
     {"ik", <<"Inupiak"/utf8>>},
     {"io", <<"Ido"/utf8>>},
     {"is", <<"Icelandic"/utf8>>},
     {"it", <<"Italian"/utf8>>},
     {"iu", <<"Inuktitut"/utf8>>},
     {"ja", <<"Japanese"/utf8>>},
     {"jv", <<"Javanese"/utf8>>},
     {"ka", <<"Georgian"/utf8>>},
     {"ki", <<"Kikuyu"/utf8>>},
     {"kj", <<"Kuanyama"/utf8>>},
     {"kk", <<"Kazakh"/utf8>>},
     {"kl", <<"Kalaallisut; Greenlandic"/utf8>>},
     {"km", <<"Khmer; Cambodian"/utf8>>},
     {"kn", <<"Kannada"/utf8>>},
     {"ko", <<"Korean"/utf8>>},
     {"ks", <<"Kashmiri"/utf8>>},
     {"ku", <<"Kurdish"/utf8>>},
     {"kv", <<"Komi"/utf8>>},
     {"kw", <<"Cornish"/utf8>>},
     {"ky", <<"Kirghiz"/utf8>>},
     {"la", <<"Latin"/utf8>>},
     {"lb", <<"Letzeburgesch"/utf8>>},
     {"ln", <<"Lingala"/utf8>>},
     {"lo", <<"Lao; Laotian"/utf8>>},
     {"lt", <<"Lithuanian"/utf8>>},
     {"lv", <<"Latvian; Lettish"/utf8>>},
     {"mg", <<"Malagasy"/utf8>>},
     {"mh", <<"Marshall"/utf8>>},
     {"mi", <<"Maori"/utf8>>},
     {"mk", <<"Macedonian"/utf8>>},
     {"ml", <<"Malayalam"/utf8>>},
     {"mn", <<"Mongolian"/utf8>>},
     {"mo", <<"Moldavian"/utf8>>},
     {"mr", <<"Marathi"/utf8>>},
     {"ms", <<"Malay"/utf8>>},
     {"mt", <<"Maltese"/utf8>>},
     {"my", <<"Burmese"/utf8>>},
     {"na", <<"Nauru"/utf8>>},
     {"nb", <<"Norwegian Bokmål"/utf8>>},
     {"nd", <<"Ndebele, North"/utf8>>},
     {"ne", <<"Nepali"/utf8>>},
     {"ng", <<"Ndonga"/utf8>>},
     {"nl", <<"Dutch"/utf8>>},
     {"nn", <<"Norwegian Nynorsk"/utf8>>},
     {"no", <<"Norwegian"/utf8>>},
     {"nr", <<"Ndebele, South"/utf8>>},
     {"nv", <<"Navajo"/utf8>>},
     {"ny", <<"Chichewa; Nyanja"/utf8>>},
     {"oc", <<"Occitan; Provençal"/utf8>>},
     {"om", <<"(Afan) Oromo"/utf8>>},
     {"or", <<"Oriya"/utf8>>},
     {"os", <<"Ossetian; Ossetic"/utf8>>},
     {"pa", <<"Panjabi; Punjabi"/utf8>>},
     {"pi", <<"Pali"/utf8>>},
     {"pl", <<"Polish"/utf8>>},
     {"ps", <<"Pashto, Pushto"/utf8>>},
     {"pt", <<"Portuguese"/utf8>>},
     {"qu", <<"Quechua"/utf8>>},
     {"rm", <<"Rhaeto-Romance"/utf8>>},
     {"rn", <<"Rundi; Kirundi"/utf8>>},
     {"ro", <<"Romanian"/utf8>>},
     {"ru", <<"Russian"/utf8>>},
     {"rw", <<"Kinyarwanda"/utf8>>},
     {"sa", <<"Sanskrit"/utf8>>},
     {"sc", <<"Sardinian"/utf8>>},
     {"sd", <<"Sindhi"/utf8>>},
     {"se", <<"Northern Sami"/utf8>>},
     {"sg", <<"Sango; Sangro"/utf8>>},
     {"si", <<"Sinhalese"/utf8>>},
     {"sk", <<"Slovak"/utf8>>},
     {"sl", <<"Slovenian"/utf8>>},
     {"sm", <<"Samoan"/utf8>>},
     {"sn", <<"Shona"/utf8>>},
     {"so", <<"Somali"/utf8>>},
     {"sq", <<"Albanian"/utf8>>},
     {"sr", <<"Serbian"/utf8>>},
     {"ss", <<"Swati; Siswati"/utf8>>},
     {"st", <<"Sesotho; Sotho, Southern"/utf8>>},
     {"su", <<"Sundanese"/utf8>>},
     {"sv", <<"Swedish"/utf8>>},
     {"sw", <<"Swahili"/utf8>>},
     {"ta", <<"Tamil"/utf8>>},
     {"te", <<"Telugu"/utf8>>},
     {"tg", <<"Tajik"/utf8>>},
     {"th", <<"Thai"/utf8>>},
     {"ti", <<"Tigrinya"/utf8>>},
     {"tk", <<"Turkmen"/utf8>>},
     {"tl", <<"Tagalog"/utf8>>},
     {"tn", <<"Tswana; Setswana"/utf8>>},
     {"to", <<"Tonga"/utf8>>},
     {"tr", <<"Turkish"/utf8>>},
     {"ts", <<"Tsonga"/utf8>>},
     {"tt", <<"Tatar"/utf8>>},
     {"tw", <<"Twi"/utf8>>},
     {"ty", <<"Tahitian"/utf8>>},
     {"ug", <<"Uighur"/utf8>>},
     {"uk", <<"Ukrainian"/utf8>>},
     {"ur", <<"Urdu"/utf8>>},
     {"uz", <<"Uzbek"/utf8>>},
     {"vi", <<"Vietnamese"/utf8>>},
     {"vo", <<"Volapuk"/utf8>>},
     {"wa", <<"Walloon"/utf8>>},
     {"wo", <<"Wolof"/utf8>>},
     {"xh", <<"Xhosa"/utf8>>},
     {"yi", <<"Yiddish"/utf8>>},
     {"yo", <<"Yoruba"/utf8>>},
     {"za", <<"Zhuang"/utf8>>},
     {"zh", <<"Chinese"/utf8>>},
     {"zu", <<"Zulu"/utf8>>}
].

