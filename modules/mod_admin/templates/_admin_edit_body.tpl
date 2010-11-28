{% with m.rsc[id] as r %}
{% if r.language or m.modules.info.mod_translation.enabled %}
{% with r.language|default:[z_language] as r_language %}
<div class="item-wrapper">
	<div class="translations ui-tabs" id="{{ #tabs }}">
		{% include "_admin_translation_tabs.tpl" prefix=#prefix r_language=r_language %}
		
		{% for code,lang in m.config.i18n.language_list.list|default:[[z_language,[]]] %}
		    <div id="{{ #prefix }}-{{ code }}" class="ui-tabs-hide">
		        <fieldset class="admin-form">
		            {% button action={zmedia id=id media_div_id=#media subject_id=id} text=_"Add media to body" id="zmedia-open-dialog" style="display:none" %}
		            {% wire action={event type='named' name="zmedia" action={zmedia id=id media_div_id=#media subject_id=id}} %}
		            {% wire action={event type='named' name="zlink" action={dialog_open title="Add link" template="_action_dialog_zlink.tpl"}} %}

		            <div class="form-item clearfix">
		                {% if is_editable %}
		                <textarea rows="10" cols="10" id="rsc-body${{ code }}" name="body${{ code }}" class="body tinymce-init">{{ r.translation[code].body|escape }}</textarea>
		                {% else %}
		                {{ r.translation[code].body }}
		                {% endif %}
		            </div>
		        </fieldset>

		        {% include "_admin_save_buttons.tpl" %}

		    </div>
		{% endfor %}
	</div>
</div>
{% endwith %}
{% else %} {# non-multilanguage content and translation module disabled #}

<div class="item-wrapper" id="sort-category">
    <h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
        <a name="category"></a>
        <span class="title">{_ Content _}</span>
        <span class="arrow">{_ make smaller _}</span>
    </h3>
    <div class="item clearfix admin-form">
        <fieldset class="admin-form">
            {% button action={zmedia id=id media_div_id=#media subject_id=id} text=_"Add media to body" id="zmedia-open-dialog" style="display:none" %}
            {% wire action={event type='named' name="zmedia" action={zmedia id=id media_div_id=#media subject_id=id}} %}
            {% wire action={event type='named' name="zlink" action={dialog_open title="Add link" template="_action_dialog_zlink.tpl"}} %}

            <div class="form-item clearfix">
                {% if is_editable %}
                <textarea rows="10" cols="10" id="rsc-body" name="body" class="body tinymce-init">{{ r.body|escape }}</textarea>
                {% else %}
                {{ r.body }}
                {% endif %}
            </div>
        </fieldset>

        {% include "_admin_save_buttons.tpl" %}
    </div>
</div>

{% endif %}
{% endwith %}
