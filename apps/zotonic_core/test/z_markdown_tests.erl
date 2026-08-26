-module(z_markdown_tests).

-include_lib("eunit/include/eunit.hrl").

triplequote_test() ->
    Text = <<"
Hello

```html
This is <code>foo</code>!
```

Dag
">>,
    Html = z_markdown:to_html(Text),
    ?assertEqual(
        <<"<p>Hello</p>
<pre lang=\"html\" class=\"notranslate\"><code class=\"notranslate language-html\">This is &lt;code&gt;foo&lt;/code&gt;!
</code></pre>
<p>Dag</p>">>, Html).

quadruplequote_test() ->
    Text = <<"
Hello

````html
```
This is <code>foo</code>!
````

Dag
">>,
    Html = z_markdown:to_html(Text),
    ?assertEqual(
        <<"<p>Hello</p>
<pre lang=\"html\" class=\"notranslate\"><code class=\"notranslate language-html\">```
This is &lt;code&gt;foo&lt;/code&gt;!
</code></pre>
<p>Dag</p>">>, Html).

code_to_markdown_test() ->
    Html = <<"<pre><code class=\"language-django\">&lt;div class=\"form-group\"&gt;
</code></pre>">>,
    Markdown = z_markdown:to_markdown(Html),
    ?assertEqual(<<"```django\n<div class=\"form-group\">\n```">>, Markdown).

lines_test() ->
    Text = <<"A sentence

Another sentence">>,
    Html = <<"<p>A sentence</p>\n<p>Another sentence</p>">>,
    ?assertEqual(Html, z_markdown:to_html(Text)),
    ?assertEqual(Text, z_string:trim(z_markdown:to_markdown(Html))),
    ok.

table_test() ->
    Text = <<"A sentence

| Hallo | Daar | Enzo |
| -----: | :---: | :--- |
| Foo | *Bår* | **Baz** |
| A\\|a | *Bbb* | CcC |

Another sentence
"/utf8>>,
    Html = <<"<p>A sentence</p>
<table role=\"table\" class=\"table\"><thead><tr><th align=\"right\">Hallo</th><th align=\"center\">Daar</th><th align=\"left\">Enzo</th></tr></thead><tbody><tr><td align=\"right\">Foo</td><td align=\"center\"><em>Bår</em></td><td align=\"left\"><strong>Baz</strong></td></tr><tr><td align=\"right\">A|a</td><td align=\"center\"><em>Bbb</em></td><td align=\"left\">CcC</td></tr></tbody></table>
<p>Another sentence</p>"/utf8>>,

    ?assertEqual(Html, z_string:trim(z_markdown:to_html(Text))),

    Text2 = <<"A sentence

| Hallo | Daar  | Enzo    |
| ----: | :---: | :------ |
|   Foo | *Bår* | **Baz** |
|  A\\|a | *Bbb* | CcC     |

Another sentence"/utf8>>,

    ?assertEqual(Text2, z_string:trim(z_markdown:to_markdown(Html))),
    ok.

%% Test CommonMark delimiter handling around alphanumeric characters.
inline_markup_test() ->
    %% Opening delimiter after alphanumeric: no emphasis/strong/del is applied.
    ?assertEqual(<<"<p>foo_bar</p>">>, z_markdown:to_html(<<"foo_bar">>)),
    ?assertEqual(<<"<p>test*value</p>">>, z_markdown:to_html(<<"test*value">>)),
    ?assertEqual(<<"<p>abc~~def</p>">>, z_markdown:to_html(<<"abc~~def">>)),
    ?assertEqual(<<"<p>test**value</p>">>, z_markdown:to_html(<<"test**value">>)),
    ?assertEqual(<<"<p>test___value</p>">>, z_markdown:to_html(<<"test___value">>)),
    %% Intraword underscores are not applied.
    ?assertEqual(<<"<p>_foo_bar</p>">>, z_markdown:to_html(<<"_foo_bar">>)),
    %% Strong and strikethrough can close immediately before alphanumeric text.
    ?assertEqual(<<"<p><strong>foo</strong>bar</p>">>, z_markdown:to_html(<<"**foo**bar">>)),
    ?assertEqual(<<"<p><del>foo</del>bar</p>">>, z_markdown:to_html(<<"~~foo~~bar">>)),
    %% A delimiter followed by a space still triggers markup.
    ?assertEqual(<<"<p><em>foo</em> bar</p>">>, z_markdown:to_html(<<"_foo_ bar">>)),
    ?assertEqual(<<"<p><strong>foo</strong> bar</p>">>, z_markdown:to_html(<<"**foo** bar">>)),
    ?assertEqual(<<"<p><del>foo</del> bar</p>">>, z_markdown:to_html(<<"~~foo~~ bar">>)),
    ok.

compatibility_entry_point_test() ->
    Markdown = <<"Hello **world**">>,
    ?assertEqual(z_markdown:to_html(Markdown), apply(markdown, conv, [Markdown])).

legacy_to_markdown_options_test() ->
    Html = <<"<p>Hello</p><table><tr><td>Left</td><td>Right</td></tr></table>">>,
    ?assertEqual(
        <<"Hello\n\nLeft | Right">>,
        z_markdown:to_markdown(Html, [no_html, no_tables])).

email_preset_test() ->
    Html = <<"<p>Read <a href=\"https://example.test/update\">the update</a>.</p>"
             "<div style=\"display:none\">Hidden preheader</div>"
             "<table role=\"presentation\"><tr><td>Left</td><td>Right</td></tr></table>">>,
    ?assertEqual(
        <<"Read the update <https://example.test/update>.\n\nLeft | Right">>,
        markupz:to_markdown(Html, email)).
