# zotonic_mod_bootstrap

This module bundles Bootstrap 5 assets for Zotonic.

Included:
- Bootstrap 5 SCSS sources under `priv/lib-src/bootstrap/scss`.
- Compiled CSS under `priv/lib/bootstrap/css`.
- Bootstrap 5 JavaScript under `priv/lib/bootstrap/js`.
- Bootstrap 3 compatibility shims under `priv/lib-src/bootstrap/compat` to keep legacy template classes working.
- Standalone Bootstrap 3 compatibility CSS as `priv/lib/bootstrap/css/bootstrap3-compat.css` and `bootstrap3-compat.min.css`, for sites that include their own Bootstrap 5 CSS separately.

## Bootstrap 3 compatibility CSS

The default `bootstrap.css` build includes Bootstrap 5 and the Bootstrap 3 compatibility layer.

Use `bootstrap5.css` if you need the plain Bootstrap 5 build without the
compatibility layer.

Sites that include their own Bootstrap 5 build can include only the compatibility layer after their Bootstrap 5 CSS:

```django
{% lib
    "css/my-bootstrap5.css"
    "/lib/bootstrap/css/bootstrap3-compat.css"
%}
```

For production, either include the minified file directly:

```django
{% lib
    "css/my-bootstrap5.min.css"
    "/lib/bootstrap/css/bootstrap3-compat.min.css"
%}
```

Or let Zotonic select the minified assets with the `minify` option:

```django
{% lib
    "css/my-bootstrap5.css"
    "/lib/bootstrap/css/bootstrap3-compat.css"
    minify
%}
```

The compatibility file must be loaded after Bootstrap 5, because it adds Bootstrap 3 class names and overrides Bootstrap 5 behavior where needed for legacy templates.

Licensing:
- Bootstrap is licensed under the MIT license. See `BOOTSTRAP-LICENSE` and `BOOTSTRAP-README.md`.
