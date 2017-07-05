{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Introduction for confirmation email _}{% endblock %}
{% block widget_id %}edit-survey-email-text{% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
	<p class="help-block">{_ Introduction for confirmation email _}</p>
	<fieldset class="admin-form">
		<div class="form-item clearfix">
			{% if is_editable %}
				<textarea rows="10"
				    id="email_text_html{{ lang_code_for_id }}"
				    name="email_text_html{{ lang_code_with_dollar }}"
				    class="input-block-level body tinymce-init"
				    {% include "_language_attrs.tpl" language=lang_code %}>{{ id.email_text_html[lang_code] |escape }}</textarea>
			{% else %}
				{{ id.email_text_html[lang_code]  }}
			{% endif %}
		</div>
	</fieldset>
{% endblock %}
