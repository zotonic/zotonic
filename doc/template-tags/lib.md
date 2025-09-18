See also

[mod\_development](/id/doc_module_mod_development).

Combine css and javascript includes in a single request.

Generates a `<link /\>` or `<script /\>` element for including the given libraries. This combines all css and javascript files in one request, saving multiple roundtrips to the server.

Example:


```erlang
{% lib
      "css/zp-compressed.css"
      "css/zp-admin.css"
      "css/zp-wysiwyg.css"
      "css/zp-dialog.css"
      "css/zp-formreplace.css"
      "css/zp-finder.css"
      "css/zp-growl.css"
  %}
```

Will output:


```erlang
<link href="/lib/css/zp-compressed~zp-admin~zp-wysiwyg~zp-dialog~zp-formreplace~zp-finder~zp-growl~63417066183.css" type="text/css" media="all" rel="stylesheet" />
```

The number at the end is the Unix modification time of the most recently changed file.

The lib tag supports optional arguments to control the resulting html tag. The arguments are supplied after the list of files:


```erlang
{% lib
  "css/not-so-important-styles.css"
  minify
  async
%}
```

Accepted arguments are:

| Option         | Default      | Description                                                                      |
| -------------- | ------------ | -------------------------------------------------------------------------------- |
| absolute\\_url | false        | If true, prefix the generated URL with “<https:/>/\\{hostname\\}/”.              |
| title          | &lt;empty>   | Specify a value for the title attribute of the link tag.                         |
| media          | “all”        | Specify value for the media attribute of the link tag.                           |
| rel            | “stylesheet” | Specify value for the rel attribute of the link tag.                             |
| minify         |              | Force minification use as `{% lib ... minify %}`                                 |
| nocache        |              | Let the browser cacte the URL at most 1 second  `{% lib ... nocache %}`          |
| async          |              | Load css or javascript asynchronously, use as  `{% lib ... async %}`             |
| defer          |              | Load javascript in parallel and executes it after the page has finished parsing, use as `{% lib ... defer %}` |

The `` `minify` `` argument can be enabled for all lib tags by setting the config `` `site.minification_enabled` `` to a true-ish value (like `` `1` ``).

Note

The `defer` argument is set even if the `async` attribute is specified to cause legacy Web browsers that only support defer (and not async) to fall back to the defer behavior instead of the synchronous blocking behavior that is the default. See more in [W3](https://www.w3.org/TR/2011/WD-html5-20110525/scripting-1.html#attr-script-async) documentation.