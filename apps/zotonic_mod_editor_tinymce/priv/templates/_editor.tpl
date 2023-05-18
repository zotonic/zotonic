{#
This template maintains the newest tinymce version.

params:
- overrides_tpl:          (optional) template location that contains JavaScript overrides for tinymce init
- zmedia_tabs_enabled:    Tabs enabled in the link dialog for the zmedia action
- zmedia_tabs_disabled:   Tabs disabled in the link dialog for the zmedia action (defaults to ["new"])
- zlink_tabs_enabled:     Tabs enabled in the link dialog for the zlink action
- zlink_tabs_disabled:    Tabs disabled in the link dialog for the zlink action
#}

{% block _editor %}

{% wire name="zmedia"
    action={
        dialog_open
        intent="connect"
        template="_action_dialog_connect.tpl"
        title=_"Insert media"
        width="large"
        subject_id=id
        predicate=`depiction`
        is_zmedia
        tab="depiction"
        callback="window.zAdminMediaDone"
        center=0
        level=5
        autoclose
        tabs_enabled=zmedia_tabs_enabled
        tabs_disabled=zmedia_tabs_disabled|default:["new"]
    }
%}

{% wire name="zlink"
    action={
        dialog_open
        intent="select"
        template="_action_dialog_connect.tpl"
        title=_"Add link"
        width="large"
        subject_id=id
        is_zlink
        tab="find"
        callback="window.zAdminLinkDone"
        center=0
        level=5
        autoclose
        tabs_enabled=zlink_tabs_enabled
        tabs_disabled=zlink_tabs_disabled|default:["new"]
    }
%}

{% with m.editor_tinymce.version_current as version %}
{% include
    "tinymce-" ++ version ++ "/_editor.tpl"
    overrides_tpl=overrides_tpl|default:"_admin_tinymce_overrides_js.tpl"
    is_editor_include=is_editor_include
    id=id
%}
{% endwith %}

{% endblock %}
