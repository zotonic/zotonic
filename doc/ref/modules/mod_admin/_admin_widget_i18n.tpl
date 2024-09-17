{% extends "admin_edit_widget_i18n.tpl" %}

{#
    More complex widget for editing language-depend content.
    Look at "doc/admin/tpl/_admin_widget_std.tpl" before start to develop i18n-enabled widget.

    Depending on whether mod_translation enabled or disabled:
	* if enabled, this widget will be displayed as tabs. See /admin/edit/1 top;
	* if disabled - will be displayed as standard widget.
	  See /admin/edit/1 no-tabs widgets, like "address", etc.

    Before you delete this comment, you should understand: only blocks "widget_content" and "widget_i18n_tab_class"
    provide localization. They are will be rendered several times: one time per every enabled language.
    In other words, rendered once per lang tab.
    All other blocks are rendered only once - when page loads.
#}

{# See doc/admin/tpl/_admin_widget_std.tpl for block description. #}
   NOTE: displayed only then mod_translation is disabled. #}
{% block widget_title %}
{_ Basic _}
<div class="widget-header-tools"></div>
{% endblock %}


{# Optional CSS-classes for widget container #}
{% block widget_i18n_tab_class %}item{% endblock %}


{# See doc/admin/tpl/_admin_widget_std.tpl for block description. #}
{% block widget_before %}{% endblock %}


{# See doc/admin/tpl/_admin_widget_std.tpl for block description.
   See variables in doc/tpl/admin/README.i18n for i18n variables description.
   Tags inside this block should be ready for using in i18n and non-i18n enviroments. #}
{% block widget_content %}
	<fieldset class="admin-form">
	    <div>
		{# Then i18n is disabled, variables "lang_code", "lang_code_with_dollar" and others are undefined,
		   so following ids and names goes to unlocalized identifiers. #}
		<label for="{{ #field }}{{ lang_code_with_dollar }}">{_ Title _} {{ lang_code_with_brackets }}</label>

		{# INPUT-tag: Look at name and value attributes: value is rendered using "if" filter: #}
		<input class="form-control" type="text" id="{{ #field }}{{ lang_code_with_dollar }}" name="title{{ lang_code_with_dollar }}"
			value="{{ is_i18n|if : id.translation[lang_code].title : id.title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}/>
	    </div>
	</fieldset>
{% endblock %}


{# See doc/admin/tpl/_admin_widget_std.tpl for block description. #}
{% block widget_after %}
    <script language="text/javascript" nonce="{{ m.req.csp_nonce }}">
	alert("Hello World!");
    </script>
{% endblock %}


{# Used only then mod_translation is disabled.
   See doc/admin/tpl/_admin_widget_std.tpl for description.
   NOTE: by now the content presence in this block is mandatory. #}
{% block widget_show_minimized %}false{% endblock %}
