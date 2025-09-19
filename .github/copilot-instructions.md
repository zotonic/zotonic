# Project Overview

This project is a content management system and framework. It is used to
build websites that are information-rich, have real-time functionality or special
security or connectivity needs.

The project is an Erlang umbrella application. Though the individual modules can also
be used as modules pulled from Hex.pm using rebar dependencies.

## Folder Structure

- `/apps`: Contains Erlang applications which implement modules and the core Zotonic system.
- `/doc`: Contains release-notes and template tag documentation.
- `/apps_user`: Contains Erlang applications that are user supplied modules or sites.
- `/bin`: Contains scripts for starting and managing Zotonic
- `/src`: Contains the source code for the Zotonic umbrella application
- `/docker`: Contains Docker files and configs
- `/cloud-init`: Contains initialization and config files for cloud servers
- `/nix`: Contains NixOS files and configs

## Libraries and Frameworks

- Cotonic for the user interface and client/server communication
- MQTT for the client/server protocol
- Bootstrap as the css framework used for core modules
- Cowboy for the HTTP server
- PostgreSQL for storage

## Structure of module and site applications

Each site and module is an Erlang application. They must have a .app.src file in
their src directory and a rebar.config file in the root directory.

A module name *must* start with `zotonic_mod_<modulename>` and *must* have the
following files:

- `rebar.config`
- `src/zotonic_mod_<modulename>.app.src`
- `src/mod_<modulename>.erl`

The directory structure of a Zotonic module application is as follows:

```
zotonic_mod_example/
  rebar.config
  src/
    zotonic_mod_example.app.src
    mod_example.erl
    actions/
        action_example_somename.erl
    filters/
        filter_foobar.erl
    scomps/
        scomp_example_somename.erl
    validators/
        validator_example_somename.erl
    models/
        m_foobar.erl
    controllers/
        controller_foobar.erl
    support/
        example_routines.erl
  include/
  priv/
    dispatch/
    lib/
        js/
        css/
        images/
    lib-src/
    templates/
    translations/
      en.po
      nl.po
```

Validators, scomps (screen components) and actions have the name of the module in their
module name. This makes it possible to replace the validator with other code from a higher
priority module or site.

In priv/lib-src we have the source code of CSS and Javascript code, which is placed in priv/lib.

The directory structure of a Zotonic site application is very similar to a module, but it *must*
have a `priv/zotonic_site` configuration file:

```
sitename/
  rebar.config
  src/
    sitename.app.src
    sitename.erl
    actions/
        action_sitename_somename.erl
    filters/
        filter_foobar.erl
    scomps/
        scomp_sitename_somename.erl
    validators/
        validator_sitename_somename.erl
    models/
        m_foobar.erl
    controllers/
        controller_foobar.erl
    support/
        example_routines.erl
  include/
  priv/
    zotonic_site.config
    dispatch/
    lib/
        js/
        css/
        images/
    lib-src/
    templates/
    translations/
      en.po
      nl.po
```

The `priv/zotonic_site.config` file can be in different formats. The extension depends on the format used
in the config file. The supported formats are:

 - `.config` for Erlang (`priv/zotonic_site.config`)
 - `.json` for JSON (`priv/zotonic_site.json`)
 - `.yaml` or `.yml` for YAML (`priv/zotonic_site.yml`)

## Templates

Templates are always stored in the `priv/templates` directory. They have the file extension `.tpl`.
The syntax of templates is very similar to the Django template language.

In templates care should be taken that values from `q` are not directly shown in `{{ ... }}` constructs
but escaped or otherwise sanitized.

Values from the `m.rsc` model are sanitized and can be displayed directly. Values from other models are
not sanitized and must be escaped or otherwise sanitized.

Files in the directory `priv/templates/static` can be served as-is. If a file in this directory has
the extension `.tpl` then it *must* be a valid template.

Favor semantic HTML and accessibility.

- Use semantic tags: `header`, `nav`, `main`, `section`, `article`, `footer`, etc.
- Prefer Zotonic template constructs ({% block %}, {% if %}, {% for %}, etc.) for logic.
- Avoid excessive divs and inline styles.
- Add `aria-` attributes and `alt` text to images for accessibility.
- Use lowercase for HTML tags and attributes.
