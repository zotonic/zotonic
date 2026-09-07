-module(filter_get_admin_url).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "site_management", "routing_and_redirects", "url"]
}).
-moduledoc("
Return the absolute administration URL for a Zotonic site.

The result is `undefined` when the site has no administration dispatch rule.

For example:

```django
<a href=\"{{ `mysite`|get_admin_url }}\">Open administration</a>
```
").

-export([get_admin_url/2]).

get_admin_url(Site, _Context) ->
    SiteContext = z:c(Site),
    case z_dispatcher:url_for(admin, SiteContext) of
        undefined ->
            undefined;
        U ->
            z_dispatcher:abs_url(U, SiteContext)
    end.
