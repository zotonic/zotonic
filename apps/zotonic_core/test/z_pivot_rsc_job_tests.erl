%% @hidden
-module(z_pivot_rsc_job_tests).

-include_lib("eunit/include/eunit.hrl").


escaped_markup_stays_searchable_test() ->
    Text = <<"<p>Use <code>&lt;script&gt;</code> and &lt;link&gt;.</p>"
             "<h2>LSP</h2><p>Sublime &amp; templates: A &lt; B.</p>">>,
    Plain = z_pivot_rsc_job:tsv_plain_text(Text),
    ?assertEqual(nomatch, binary:match(Plain, [<<"<">>, <<">">>])),
    ?assertNotEqual(nomatch, binary:match(Plain, <<"script">>)),
    ?assertNotEqual(nomatch, binary:match(Plain, <<"LSP">>)),
    ?assertNotEqual(nomatch, binary:match(Plain, <<"Sublime & templates: A B.">>)).
