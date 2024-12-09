{% with
    m.modules.active.mod_translation,
    r_language|default:(m.rsc[id].language)|default:[z_language]
    as
    is_i18n,
    r_language
%}
{% with edit_language|default:z_language as edit_language %}
{% with edit_language|member:r_language|if:edit_language:(r_language[1]) as edit_language %}
{% if is_i18n %}
	{% block widget_before %}{% endblock %}
	<div class="widget {% if in_dialog %}dialog-widget{% endif %} translations tabbable {% if show_header %}do_adminwidget{% endif %}{% block widget_class %}{% endblock %}"
		 data-adminwidget='{ "minifiedOnInit": {% block widget_show_minimized %}false{% endblock %}, "minifier": {% if show_opened or in_dialog %}false{% else %}true{% endif %} }'
		 id="{% block widget_id %}{% endblock %}">

		{% if not in_dialog and not noheader %}
            {% block widget_header %}
                {% if show_header %}
                <div class="widget-header">
                    {% block widget_title %}<div class="widget-header-tools"></div>{% endblock %}
                </div>
                {% endif %}
            {% endblock %}
        {% endif %}

		{% include "_admin_translation_tabs.tpl" prefix=#prefix r_language=r_language %}

		<div class="tab-content widget-content nolang_before">{% block widget_content_nolang_before %}{% endblock %}</div>

		<div class="tab-content widget-content">
			{% for lang_code,lang in m.translation.language_list_editable %}
    			{# to define some helper vars that will be useful in widget_content: #}
    			{% with "$" ++ lang_code,
    					"(" ++ lang_code ++ ")",
    					"--" ++ lang_code
    				as  lang_code_with_dollar,
    					lang_code_with_brackets,
    					lang_code_for_id
    			%}
        			<div id="{{ #prefix }}-{{ lang_code }}" class="tab-pane {% if lang_code == edit_language %}active{% endif %} edit-language-{{ lang_code }} {% block widget_i18n_tab_class %}{% endblock %}">
        				{% block widget_content %}{% endblock %}
        			</div>
    			{% endwith %}
			{% endfor %}
		</div>

		<div class="tab-content widget-content nolang">{% block widget_content_nolang %}{% endblock %}</div>
	</div>
	{% block widget_after %}{% endblock %}

{% else %}
	{# non-multilanguage content and translation module disabled #}
	{% with z_language as lang_code %}
		{% include "admin_edit_widget_std.tpl" %}
	{% endwith %}
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
