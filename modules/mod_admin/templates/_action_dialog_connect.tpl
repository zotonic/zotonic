{% with callback|default:q.callback|default:"window.zAdminConnectDone" as callback %}
{% with language|default:q.language|default:z_language as language %}
{% with actions|default:[] as actions %}
{% with stay or callback or subject_id as stay %}
{% with tabs_enabled|default:(m.config.mod_admin.rsc_dialog_tabs.value|split:",") as tabs_enabled %}
{% with tab|default:q.tab|default:(tabs_enabled|first)|default:"find" as tab %}
<ul class="nav nav-pills">
    {% block tabs %}
	{% if in_sorter == "category" %}
	    {% if "new"|member:tabs_enabled %}
            <li class="active">
                <a data-toggle="tab" href="#{{ #tab }}-new">{_ New Page _}</a>
            </li>
        {% endif %}
	{% else %}
	    {% if not tabs_enabled or "depiction"|member:tabs_enabled %}
    		{% if q.is_zmedia %}
                <li {% if tab == "depiction" %}class="active"{% endif %}>
                    <a data-toggle="tab" href="#{{ #tab }}-depiction">{_ Attached media _}</a>
                </li>
            {% endif %}
		{% endif %}
		{% if not tabs_enabled or "find"|member:tabs_enabled %}
            <li {% if tab == "find" and not q.is_zmedia %}class="active"{% endif %}>
                <a data-toggle="tab" href="#{{ #tab }}-find">{_ Find Page _}</a>
            </li>
        {% endif %}
        {% if not tabs_enabled or "new"|member:tabs_enabled %}
            {% if predicate.name /= "depiction" %}
            <li {% if tab == "new" %}class="active"{% endif %}>
                <a data-toggle="tab" href="#{{ #tab }}-new">{_ New Page _}</a>
            </li>
            {% endif %}
        {% endif %}
        {% if not tabs_enabled or "upload"|member:tabs_enabled %}
            <li {% if tab == "upload" and not q.is_zmedia %}class="active"{% endif %}>
                <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload _}</a>
            </li>
        {% endif %}
        {% if not tabs_enabled or "url"|member:tabs_enabled %}
            <li {% if tab == "url" %}class="active"{% endif %}>
                <a data-toggle="tab" href="#{{ #tab }}-url">{_ URL _}</a>
            </li>
	    {% endif %}
   	    {% all include "_media_upload_tab.tpl" tab=#tab %}
	{% endif %}
    {% endblock %}
</ul>

<div class="tab-content" id="dialog-connect-panels">
    {% block tabs_content %}
	{% if in_sorter == "category" %}
	    {# only one tab, so no conditional #}
            {% include "_action_dialog_connect_tab_new.tpl" tab=#tab predicate=predicate subject_id=subject_id is_active
                    title="" cat=m.rsc.category.id nocatselect %}
	{% else %}

	    {% if not tabs_enabled or "depiction"|member:tabs_enabled %}
            {% if q.is_zmedia %}
                {% include "_action_dialog_connect_tab_depictions.tpl" tab=#tab predicate=predicate subject_id=subject_id is_active=(tab == "depiction") title="" %}
            {% endif %}
        {% endif %}

        {% if not tabs_enabled or "find"|member:tabs_enabled %}
            {% include "_action_dialog_connect_tab_find.tpl" tab=#tab predicate=predicate subject_id=subject_id 
                        is_active=(not q.is_zmedia and tab == "find") title="" %}
        {% endif %}

        {% if not tabs_enabled or "new"|member:tabs_enabled %}
            {% if predicate.name /= "depiction" %}
                {% include "_action_dialog_connect_tab_new.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" 
                            is_active=(tab == "new") %}
            {% endif %}
        {% endif %}

		{% with "action_admin_dialog_media_upload" as delegate %}
            {% if not tabs_enabled or "upload"|member:tabs_enabled %}
                {% include "_action_dialog_media_upload_tab_upload.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" 
						is_active=(predicate.name=="depiction" and not is_zmedia and tab == "upload") %}
			{% endif %}

			{% if not tabs_enabled or "url"|member:tabs_enabled %}
                {% include "_action_dialog_media_upload_tab_url.tpl" tab=#tab predicate=predicate subject_id=subject_id is_active=(tab == "url") title="" %}
            {% endif %}

            {% all include "_media_upload_panel.tpl" tab=#tab predicate=predicate subject_id=subject_id title="" delegate=delegate %}
		{% endwith %}
	{% endif %}
    {% endblock %}
</div>
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
