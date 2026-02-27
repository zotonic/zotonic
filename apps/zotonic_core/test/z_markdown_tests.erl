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


<pre lang=\"html\" class=\"notranslate\"><code class=\"notranslate language-html\">This is &lt;code&gt;foo&lt;/code&gt;!</code></pre>

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


<pre lang=\"html\" class=\"notranslate\"><code class=\"notranslate language-html\">```\nThis is &lt;code&gt;foo&lt;/code&gt;!</code></pre>

<p>Dag</p>">>, Html).


rst_code_to_markdown_test() ->
    Html = <<"
<div class=\"highlight-django notranslate\"><div class=\"highlight\"><pre><span></span><span class=\"x\">&lt;div class=\"form-group\"&gt; &lt;!-- form-group has class \"has-error\" if validation fails --&gt;</span>
</pre></div>
</div>">>,
    MD = z_markdown:to_markdown(Html),
    ?assertEqual(<<"```django\n<div class=\"form-group\"> <!-- form-group has class \"has-error\" if validation fails -->\n```\n\n">>, MD).


lines_test() ->
    Text = <<"A sentence

Another sentence">>,
    Html = <<"<p>A sentence</p>\n\n<p>Another sentence</p>">>,
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

<table role=\"table\" class=\"table\">
  <thead>
    <tr><th align=\"right\">Hallo</th><th align=\"center\">Daar</th><th align=\"left\">Enzo</th></tr>
  </thead>
  <tbody>
    <tr><td align=\"right\">Foo</td><td align=\"center\"><em>Bår</em></td><td align=\"left\"><strong>Baz</strong></td></tr>
    <tr><td align=\"right\">A|a</td><td align=\"center\"><em>Bbb</em></td><td align=\"left\">CcC</td></tr>
  </tbody>
</table>

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

%% Test that inline emphasis/strong/del delimiters are not applied when the
%% opening delimiter immediately follows an alphanumeric character, or when
%% the closing delimiter is immediately followed by an alphanumeric character.
inline_markup_test() ->
    %% Opening delimiter after alphanumeric: no emphasis/strong/del should be applied.
    ?assertEqual(<<"<p>foo_bar</p>">>, z_markdown:to_html(<<"foo_bar">>)),
    ?assertEqual(<<"<p>test*value</p>">>, z_markdown:to_html(<<"test*value">>)),
    ?assertEqual(<<"<p>abc~~def</p>">>, z_markdown:to_html(<<"abc~~def">>)),
    ?assertEqual(<<"<p>test**value</p>">>, z_markdown:to_html(<<"test**value">>)),
    ?assertEqual(<<"<p>test___value</p>">>, z_markdown:to_html(<<"test___value">>)),
    %% Closing delimiter before alphanumeric: no emphasis/strong/del should be applied.
    ?assertEqual(<<"<p>_foo_bar</p>">>, z_markdown:to_html(<<"_foo_bar">>)),
    ?assertEqual(<<"<p>**foo**bar</p>">>, z_markdown:to_html(<<"**foo**bar">>)),
    ?assertEqual(<<"<p>~~foo~~bar</p>">>, z_markdown:to_html(<<"~~foo~~bar">>)),
    %% Mixed: delimiter followed by space should still trigger markup.
    ?assertEqual(<<"<p><em>foo</em> bar</p>">>, z_markdown:to_html(<<"_foo_ bar">>)),
    ?assertEqual(<<"<p><strong>foo</strong> bar</p>">>, z_markdown:to_html(<<"**foo** bar">>)),
    ?assertEqual(<<"<p><del>foo</del> bar</p>">>, z_markdown:to_html(<<"~~foo~~ bar">>)),
    ok.
