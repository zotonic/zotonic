{% with m.modules.info.mod_translation.enabled, 
        r_language|default:(m.rsc[id].language)|default:[z_language]
   as is_i18n, 
      r_language 
%}
{% if is_i18n %}
	{% block widget_before %}{% endblock %}
	<div class="{% if in_dialog %}dialog-{% endif %}widget translations tabbable" id="{% block widget_id %}{% endblock %}">
		{% include "_admin_translation_tabs.tpl" prefix=#prefix r_language=r_language %}
		<div class="tab-content widget-content">
			{% for lang_code, lang in m.config.i18n.language_list.list|default:[[z_language,[]]] %}
			{# to define some helper vars that will be usefull in widget_content: #}
			{% with ["$", lang_code]|join, ["(", lang_code, ")"]|join  as  lang_code_with_dollar, lang_code_with_brackets %}
			<div id="{{ #prefix }}-{{ lang_code }}" class="tab-pane {% if lang_code == r_language[1] %}active{% endif %} language-{{ lang_code }} {% block widget_i18n_tab_class %}{% endblock %}">
				{% block widget_content %}{% endblock %}
			</div>
			{% endwith %}
			{% endfor %}
		</div>
	</div>
	{% block widget_after %}{% endblock %}

{% else %}
	{# non-multilanguage content and translation module disabled #}
	{% include "admin_edit_widget_std.tpl" %}
{% endif %}

{% endwith %}
