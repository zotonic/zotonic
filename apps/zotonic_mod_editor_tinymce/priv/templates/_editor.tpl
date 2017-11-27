{#
This template maintains the newest tinymce version.

params:
overrides_tpl: (optional) template location that contains JavaScript overrides for tinymce init
#}

{% wire name="zmedia"
    action={
        dialog_open
        template="_action_dialog_connect.tpl"
        title=_"Insert image"
        subject_id=id
        predicate=`depiction`
        is_zmedia
        tab="depiction"
        callback="window.zAdminMediaDone"
        center=0
        autoclose
    }
%}

{% wire name="zlink"
    action={
        dialog_open
        template="_action_dialog_connect.tpl"
        title=_"Add link"
        subject_id=id
        is_zlink
        tab="find"
        callback="window.zAdminLinkDone"
        center=0
        autoclose
    }
%}

{% with
    "4.5.5",
    m.editor_tinymce.version
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
