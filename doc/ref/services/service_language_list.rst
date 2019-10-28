
.. include:: meta-language_list.rst

List all enabled languages.

Calling ``/api/translation/language_list`` returns
a list of languages, sorted on their name::

    [
        {"iso":"de","language":"Deutsch","is_enabled":true},
        {"iso":"et","language":"Eesti","is_enabled":true},
        {"iso":"en","language":"English","is_enabled":true},
        {"iso":"es","language":"Espa\u00f1ol","is_enabled":true},
        {"iso":"fr","language":"Fran\u00e7ais","is_enabled":true},
        {"iso":"nl","language":"Nederlands","is_enabled":true},
        {"iso":"pl","language":"Polski","is_enabled":true},
        {"iso":"tr","language":"T\u00fcrk\u00e7e","is_enabled":true},
        {"iso":"ru","language":"\u0420\u0443\u0441\u0441\u043a\u0438\u0439","is_enabled":true}
    ]

