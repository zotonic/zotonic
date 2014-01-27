{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-survey-feedback{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
	<p>{_ This text is shown after the survey has been submitted. _}</p>
	{% with m.rsc[id] as r %}
	{% with r.blocks.survey_feedback as blk %}
		<fieldset class="admin-form">
			<div class="form-item clearfix">
				{% if is_editable %}
					<textarea rows="10" 
					    id="block-{{ #s }}-body{{ lang_code_for_id }}" 
					    name="block-{{ #s }}-body{{ lang_code_with_dollar }}" 
					    class="input-block-level body tinymce-init" 
					    {% include "_language_attrs.tpl" language=lang_code %}>{{ blk.body[lang_code] |escape }}</textarea>
				{% else %}
					{{ blk.body[lang_code]  }}
				{% endif %}
			</div>
		</fieldset>
	{% endwith %}
	{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
	<input type="hidden" class="block-type" name="block-{{ #s }}-type" value="text" />
	<input type="hidden" class="block-type" name="block-{{ #s }}-name" value="survey_feedback" />
{% endblock %}
