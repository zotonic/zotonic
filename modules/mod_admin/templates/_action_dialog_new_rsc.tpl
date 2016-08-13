{#
params:
- tabs_enabled (optional): list of tab names: ["new", "depiction", "find",  "upload", "url"]
#}
{% if not (tabs_enabled and tabs_enabled|length == 1) %}
    <ul class="nav nav-pills">
        {% if not tabs_enabled or "new"|member:tabs_enabled %}
        <li class="active">
            <a data-toggle="tab" href="#{{ #tab }}-new">{_ Create Page _}</a>
        </li>
        {% endif %}
        {% if not tabs_enabled or "upload"|member:tabs_enabled %}
        <li>
            <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload File _}</a>
        </li>
        {% endif %}
        {% if not tabs_enabled or "url"|member:tabs_enabled %}
        <li>
            <a data-toggle="tab" href="#{{ #tab }}-url">{_ Upload by URL _}</a>
        </li>
        {% endif %}
        {% all include "_media_upload_tab.tpl" tab=#tab %}
    </ul>
{% endif %}
<div class="tab-content">
    {% if not tabs_enabled or "new"|member:tabs_enabled %}
    	{% include "_action_dialog_connect_tab_new.tpl" tab=#tab is_active %}
    {% endif %}
    {% if not tabs_enabled or "upload"|member:tabs_enabled %}
    	{% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab %}
    {% endif %}
    {% if not tabs_enabled or "url"|member:tabs_enabled %}
    	{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab %}
    {% endif %}
    {% all include "_media_upload_panel.tpl" tab=#tab %}
</div>
