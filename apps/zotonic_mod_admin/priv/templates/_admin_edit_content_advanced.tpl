{% extends "admin_edit_widget_std.tpl" %}

{# Widget to edit some advanced rsc props #}

{% block widget_title %}
{_ Advanced _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-advanced{% endblock %}

{% block widget_content %}
    <div class="form-group label-floating">
        <input class="form-control" type="text" id="field-page-path" name="page_path" value="{{ id.page_path }}"
            {% if not id.is_editable %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=`en` %}
            placeholder="{_ Page path _} &mdash; {{ id.default_page_url|escape }}"
        >
        <label class="control-label" for="field-page-path">{_ Page path _}</label>
    </div>

    <div class="form-group">
        <label class="control-label">
            <input type="checkbox" id="field-is-page-path-multiple"
                name="is_page_path_multiple" value="1"
                {% if id.is_page_path_multiple %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %}
            />
            {_ Show page on multiple paths _}
        </label>
    </div>

    <div class="form-group label-floating">
        {% if m.acl.use.mod_admin_config %}
            <input class="form-control" type="text" id="name" name="name" value="{{ id.name }}" {% if not id.is_editable or id == 1 %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=`en` %}
            placeholder="{_ Unique name _}" 
            >
            <label class="control-label" for="field-name">{_ Unique name _}</label>
            {% validate id="name" type={name_unique id=id failure_message=_"This name is in use by another page."} %}
        {% else %}
            &nbsp;
        {% endif %}
    </div>

    {% if id.installed_by %}
        <div class="alert alert-info edit-message">{_ This resource was created by the module _} <strong>{{ m.modules.info[id.installed_by].title|default:id.installed_by }}</strong></div>
    {% endif %}

    {% if m.acl.use.mod_admin_config %}
	    {% if id.is_a.meta or not id.is_authoritative %}
	        <div class="form-group label-floating">
                <input class="form-control" type="text" id="field-uri" name="uri" value="{{ id.uri_raw }}" {% if not id.is_editable %}disabled="disabled"{% endif %}
                placeholder="{_ Unique uri _}">
                <label class="control-label" for="field-uri">{_ Unique uri _}</label>
            </div>
	    {% endif %}
    {% endif %}

    {% block edit_advanced_extra %}
    {% endblock %}
{% endblock %}
