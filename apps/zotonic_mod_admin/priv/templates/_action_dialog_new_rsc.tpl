{#
params:
- tabs_enabled (optional): list of tab names: ["new", "depiction", "find",  "upload", "url"]
#}
{% with 'create' as intent %}

{% with tabs_disabled|default:[] as tabs_disabled %}
{% if not (tabs_enabled and tabs_enabled|length == 1) %}
    <ul class="nav nav-pills" id="nav-tab" role="tablist">
        {% if (not tabs_enabled or "new"|member:tabs_enabled) and not "new"|member:tabs_disabled %}
            <li class="nav-item active">
                <button class="nav-link active" id="findnew-tab" data-bs-toggle="tab" data-bs-target="#{{ #tab }}-findnew" type="button" role="tab" aria-controls="findnew-tab-pane" aria-selected="true">{_ New Page  _}</button>

                {# <a data-toggle="tab" href="#{{ #tab }}-findnew">{_ New Page  _}</a> #}
            </li>
        {% endif %}
        {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
            <li class="nav-item">
                <button class="nav-link" id="upload-tab" data-bs-toggle="tab" data-bs-target="#{{ #tab }}-upload" type="button" role="tab" aria-controls="upload-tab-pane" aria-selected="true">{_ Upload _}</button>
                {# <a href="#{{ #tab }}-upload" class="nav-link" data-toggle="tab">{_ Upload _}</a> #}
            </li>
        {% endif %}
        {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
        <li class="nav-item">
            <button class="nav-link" id="embed-tab" data-bs-toggle="tab" data-bs-target="#{{ #tab }}-url" type="button" role="tab" aria-controls="embed-tab-pane" aria-selected="true">{_ Website or Embed _}</button>
            {# <a data-toggle="tab" href="#{{ #tab }}-url" class="nav-link">{_ Website or Embed _}</a> #}
        </li>
        {% endif %}
        {% all include "_media_upload_tab.tpl" tab=#tab %}
    </ul>
{% endif %}
<div class="tab-content">
    {% if (not tabs_enabled or "new"|member:tabs_enabled) and not "new"|member:tabs_disabled %}
        {% include "_action_dialog_connect_tab_findnew.tpl"
            tab=#tab
            is_active=`true`
            title=""
            cat=cat
            content_group=content_group
        %}
    {% endif %}
    {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
        {% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab %}
    {% endif %}
    {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
    	{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab %}
    {% endif %}
    {% all include "_media_upload_panel.tpl" tab=#tab %}
</div>
{% endwith %}

{% endwith %}
