{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Content _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-body{% endblock %}

{% block widget_content %}
{% with property|default:"body" as property %}

	{% button action={zmedia id=id media_div_id=#media subject_id=id} text=_"Add media to body" id="zmedia-open-dialog" style="display:none" %}

	<div class="form-group">
		{% if explanation %}
			<p class="help-block">{{ explanation }}</p>
		{% endif %}
		{% with is_i18n|if:id.translation[lang_code][property]:id[property] as text %}
		{% if not id or id.is_editable %}
			<textarea rows="10" cols="10" id="rsc-{{ property }}{{ lang_code_for_id }}" name="{{ property }}{{ lang_code_with_dollar }}" class="body z_editor-init form-control" {% include "_language_attrs.tpl" language=lang_code %}>{{ text|escape }}</textarea>
		{% else %}
			{{ text }}
		{% endif %}
		{% endwith %}
	</div>
{% endwith %}
{% endblock %}
