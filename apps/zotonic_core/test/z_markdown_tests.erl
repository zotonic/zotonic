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
    ?assertEqual(<<"``` django\n<div class=\"form-group\"> <!-- form-group has class \"has-error\" if validation fails -->\n```\n">>, MD).


table_test() ->
    Text = <<"

| Hallo | Daar | Enzo |
| -----: | :---: | :--- |
| Foo | *Bar* | **Baz** |
| A\\|a | *Bbb* | CcC |

">>,
    Html = <<"<table role=\"table\" class=\"table\">
  <thead>
    <tr><th align=\"right\">Hallo</th><th align=\"center\">Daar</th><th align=\"left\">Enzo</th></tr>
  </thead>
  <tbody>
    <tr><td align=\"right\">Foo</td><td align=\"center\"><em>Bar</em></td><td align=\"left\"><strong>Baz</strong></td></tr>
    <tr><td align=\"right\">A|a</td><td align=\"center\"><em>Bbb</em></td><td align=\"left\">CcC</td></tr>
  </tbody>
</table>">>,
    ?assertEqual(Html, z_markdown:to_html(Text)),

    Text2 = <<"
| Hallo | Daar  | Enzo    |
| ----: | :---: | :------ |
|   Foo | *Bar* | **Baz** |
|  A\\|a | *Bbb* | CcC     |
">>,
    ?assertEqual(Text2, z_markdown:to_markdown(Html)),
    ok.
