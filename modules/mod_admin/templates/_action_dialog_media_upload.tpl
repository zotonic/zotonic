{% with tabs_enabled|default:(m.config.mod_admin.rsc_dialog_tabs.value|split:",") as tabs_enabled %}

<ul class="nav nav-pills">
    {% if not tabs_enabled or "upload"|member:tabs_enabled %}
    <li class="active">
        <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload _}</a>
    </li>
    {% endif %}
    {% if not tabs_enabled or "url"|member:tabs_enabled %}
    <li>
        <a data-toggle="tab" href="#{{ #tab }}-url">{_ URL _}</a>
    </li>
    {% endif %}
    {% all include "_media_upload_tab.tpl" tab=#tab %}
</ul>

<div class="tab-content">
    {% if not tabs_enabled or "upload"|member:tabs_enabled %}
	   {% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab is_active %}
    {% endif %}
    {% if not tabs_enabled or "url"|member:tabs_enabled %}
       	{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab %}
    {% endif %}
    {% all include "_media_upload_panel.tpl" tab=#tab %}
</div>

{% endwith %}
