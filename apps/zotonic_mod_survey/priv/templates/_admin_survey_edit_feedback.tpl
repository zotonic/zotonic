{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
	{_ Block _}
	<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-survey-feedback{% endblock %}
{% block widget_header %}
	<input type="hidden" name="blocks[]." value="">
	<input type="hidden" name="blocks[].type" value="text">
	<input type="hidden" name="blocks[].name" value="survey_feedback">
{% endblock %}

{% block widget_content %}
	<p class="help-block">
		{_ Thank you text. _} {_ This text is shown after the form has been submitted. _}
	</p>
	{% with id.blocks.survey_feedback as blk %}
		<fieldset class="admin-form">
			<div>
				{% if id.is_editable %}
					<textarea rows="10"
					    id="block-{{ #s }}-body{{ lang_code_for_id }}"
					    name="blocks[].body{{ lang_code_with_dollar }}"
					    class="body tinymce-init form-control"
					    {% include "_language_attrs.tpl" language=lang_code %}>{{ blk.body|translation:lang_code  |escape }}</textarea>
				{% else %}
					{{ blk.body|translation:lang_code   }}
				{% endif %}
			</div>
		</fieldset>
	{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
{% endblock %}
