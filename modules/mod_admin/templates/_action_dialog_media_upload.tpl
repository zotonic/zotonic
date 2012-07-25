<ul class="nav nav-pills">
    <li class="active">
        <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload _}</a>
    </li>
    <li>
        <a data-toggle="tab" href="#{{ #tab }}-url">{_ URL _}</a>
    </li>
    {% all include "_media_upload_tab.tpl" tab=#tab %}
</ul>

<div class="tab-content">
	{% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab is_active %}
	{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab %}

    {% all include "_media_upload_panel.tpl" tab=#tab %}
</div>
