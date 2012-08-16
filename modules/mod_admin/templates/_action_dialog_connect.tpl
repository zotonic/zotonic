{% with callback|default:q.callback|default:"window.zAdminConnectDone" as callback %}
{% with language|default:q.language|default:z_language as language %}
{% with actions|default:[] as actions %}
{% with stay or callback or subject_id as stay %}
<ul class="nav nav-pills">
	{% if in_sorter == "category" %}
		<li>
			<a data-toggle="tab" class="active" href="#{{ #tab }}-new">{_ New Page _}</a>
		</li>
	{% else %}
		{% if q.is_zmedia %}
		<li class="active">
			<a data-toggle="tab" href="#{{ #tab }}-depiction">{_ Attached media _}</a>
		</li>
		{% endif %}
		<li {% if not is_zmedia %}class="active"{% endif %}>
			<a data-toggle="tab" href="#{{ #tab }}-find">{_ Find Page _}</a>
		</li>
		{% if predicate.name /= "depiction" %}
		<li>
			<a data-toggle="tab" href="#{{ #tab }}-new">{_ New Page _}</a>
		</li>
		{% endif %}
	    <li>
	        <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload _}</a>
	    </li>
	    <li>
	        <a data-toggle="tab" href="#{{ #tab }}-url">{_ URL _}</a>
	    </li>
	    {% all include "_media_upload_tab.tpl" tab=#tab %}
	{% endif %}
</ul>

<div class="tab-content" id="dialog-connect-panels">
	{% if in_sorter == "category" %}
		{% include "_action_dialog_connect_tab_new.tpl" tab=#tab predicate=predicate subject_id=subject_id 
				title="" is_active cat=m.rsc.category.id nocatselect %}
	{% else %}
		{% if q.is_zmedia %}
			{% include "_action_dialog_connect_tab_depictions.tpl" tab=#tab predicate=predicate subject_id=subject_id is_active title="" %}
		{% endif %}

		{% include "_action_dialog_connect_tab_find.tpl" tab=#tab predicate=predicate subject_id=subject_id is_active=(not is_zmedia) title="" %}

		{% include "_action_dialog_connect_tab_new.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" %}

		{% with "action_admin_dialog_media_upload" as delegate %}
			{% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" %}
			{% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" %}

		    {% all include "_media_upload_panel.tpl" tab=#tab 
		    		predicate=predicate subject_id=subject_id title="" delegate=delegate
		    %}
		{% endwith %}
	{% endif %}
</div>
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
