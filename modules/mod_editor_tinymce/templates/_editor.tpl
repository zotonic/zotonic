{#
This template maintains the newest tinymce version.

params:
overrides_tpl: (optional) template location that contains JavaScript overrides for tinymce init
#}
{% with
    "4.2.4",
    m.config.mod_editor_tinymce.version.value
    as
    newest,
    config
%}
{% with
    (config == "newest" or config|is_undefined)|if:newest:config
    as
    version
%}
{% include
    "tinymce-" ++ version ++ "/_editor.tpl"
    overrides_tpl=overrides_tpl|default:"_admin_tinymce_overrides_js.tpl"
    is_editor_include=is_editor_include
    id=id
%}
{% endwith %}
{% endwith %}