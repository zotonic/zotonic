{#
params:
- intent: purpose of this dialog 'create', 'connect', 'select', 'update'
- tabs_enabled (optional): list of tab names: ["new", "depiction", "find",  "upload", "url"]
- tabs_disabled (optional): list of tab names: []
#}
{% if not intent|member:[ 'select', 'create', 'connect', 'update' ] %}
    <p class="alert alert-danger">
        Please specify the <b>intent</b> argument when using the <b>_action_dialog_media_upload.tpl</b>.<br>
        It should be one of: <b>update</b>, <b>select</b>, <b>create</b> or <b>connect</b>
    </p>
{% else %}

{% with tabs_disables|default:[] as tabs_disabled %}
<div id="{{ #tab }}">
    {% if not (tabs_enabled and tabs_enabled|length == 1) %}
        <ul class="nav nav-pills">
            {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
            <li class="nav-item">
                <a data-bs-toggle="tab" class="nav-link active" href="#{{ #tab }}-upload">{_ Upload File _}</a>
            </li>
            {% endif %}
            {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
            <li class="nav-item">
                <a data-bs-toggle="tab" class="nav-link" href="#{{ #tab }}-url">{_ Website or Embed _}</a>
            </li>
            {% endif %}
            {% all include "_media_upload_tab.tpl" tab=#tab %}
        </ul>
    {% endif %}
    <div class="tab-content">
        {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
    	   {% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab is_active %}
        {% endif %}
        {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
           	{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab %}
        {% endif %}
        {% all include "_media_upload_panel.tpl" tab=#tab %}
    </div>
</div>
{% endwith %}

{% endif %}
