{#
params:
overrides_tpl - template location that contains JavaScript overrides for tinymce init
#}
{% include
    "tinymce-" ++ m.config.mod_editor_tinymce.version.value ++ "/_editor.tpl"
    overrides_tpl=overrides_tpl|default:"_admin_tinymce_overrides_js.tpl"
%}