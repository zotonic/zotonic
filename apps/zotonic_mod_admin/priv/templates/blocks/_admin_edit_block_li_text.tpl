{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Page block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
<fieldset>
	<div>
		{% if id.is_editable %}
			<textarea rows="10"
			    id="block-{{name}}-body{{ lang_code_for_id }}"
			    name="blocks[].body{{ lang_code_with_dollar }}"
			    class="body z_editor-init form-control"
			    {% include "_language_attrs.tpl" language=lang_code %}>{{ blk.body|translation:lang_code  |escape }}</textarea>
		{% else %}
			{{ blk.body|translation:lang_code  }}
		{% endif %}
	</div>
</fieldset>
{% endblock %}

{% block widget_content_nolang %}
{% include "_admin_edit_block_show_as.tpl" %}
{% endblock %}
