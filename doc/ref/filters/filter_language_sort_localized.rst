.. highlight:: erlang
.. include:: meta-language_sort_localized.rst

Sort a list of language codes or map with languages on their localized name in the
currently selected language. This is useful for editorial interfaces where editors
pick a language from a list.

Do not use for end-users wanting to select their own language, they might not be able
to understand the translated language names.

Return a list of ``{Code, LanguageProps}`` pairs.

LanguageProps is a map::

    #{
        fallback => [],
        language => <<"nl">>,
        language_atom => nl,
        name => <<"Nederlands">>,
        name_en => <<"Dutch">>,
        sort_key => <<"dutch">>,
        sublanguages => [
        ]
    }

After sorting the key ``name_localized`` will be added to the map::

    #{
        fallback => [],
        language => <<"nl">>,
        language_atom => nl,
        name => <<"Nederlands">>,
        name_en => <<"Dutch">>,
        name_localized => <<"Nederländska"/utf8>>,
        sort_key => <<"dutch">>,
        sublanguages => [
        ]
    }
