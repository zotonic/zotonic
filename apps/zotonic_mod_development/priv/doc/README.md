# Documentation images

`admin-development.jpg` is a full-page capture of `/admin/development` on the
local test site. It includes the settings, source editor, template debugging,
dispatch debugging, introspection, and function tracing panels.

The module's `-moduledoc` references it relative to `src/mod_development.erl`:

```markdown
![Development page](../priv/doc/admin-development.jpg)
```

Keep documentation images here with the module. The zotonicwww2 core reference
importer resolves relative image paths in its source checkout, imports the file
as a named media resource, and converts the image into a native media embed.
A stable source path reuses the resource; a changed file hash replaces its media.
No image copy belongs in the website's static library.
