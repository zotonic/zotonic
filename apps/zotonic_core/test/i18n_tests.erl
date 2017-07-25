-module(i18n_tests).

-include_lib("eunit/include/eunit.hrl").

gettext_test() ->
    X = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"">>),
    [{header, <<"header value">>}] = X,

    X2 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"nl">>}] = X2,

    X3 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"

msgid \"empty trans\"
msgstr \"\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"nl">>}] = X3,

    X4 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

#~ msgid \"en\"
#~ msgstr \"nl\"

msgid \"en\"
msgstr \"de\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"de">>}] = X4,
    ok.
