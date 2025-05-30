{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget to edit some advanced rsc props #}

{% block widget_title %}
{_ Advanced _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-advanced{% endblock %}

{% block widget_content %}
    <div class="form-group label-floating">
        <input type="text" id="field-page-path{{ lang_code_for_id }}"
            name="page_path{{ lang_code_with_dollar }}"
            placeholder="{_ Page path _} {{ lang_code_with_brackets }} &mdash; {{ id.default_page_url|escape }}"
            value="{{ (is_i18n|if : id.translation[lang_code].page_path : id.page_path)|urldecode|escape }}"
            {% if not id.is_editable %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="form-control" %}
        >
        <label class="control-label" for="field-page-path{{ lang_code_for_id }}">
            {_ Page path _} {{ lang_code_with_brackets }}
        </label>
    </div>
{% endblock %}

{% block widget_content_nolang %}
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

    <div class="form-group">
        <label class="control-label">
            <input type="checkbox" id="field-is-unfindable"
                name="is_unfindable" value="1"
                {% if id.is_unfindable %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %}
            />
            {_ Hide page from searches (depends on the search query) _}
        </label>
    </div>

    <div class="form-group label-floating">
        {% if m.acl.use.mod_admin %}
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

    {% if id.is_a.meta or not id.is_authoritative or id.uri %}
        {% if m.acl.use.mod_admin_config %}
	        <div class="form-group label-floating">
                <input class="form-control" type="text" id="field-uri" name="uri" value="{{ id.uri_raw|escape }}" {% if not id.is_editable %}disabled="disabled"{% endif %}
                placeholder="{_ Unique uri _}">
                <label class="control-label" for="field-uri">{_ Unique uri _}</label>
            </div>
        {% elseif id.uri %}
            <div class="form-group">
                <label class="control-label">{_ Unique uri _}</label>
                <p class="text-muted">{{ id.uri_raw|escape }}</p>
            </div>
	    {% endif %}
    {% endif %}

    {% block edit_advanced_extra %}
    {% endblock %}
{% endblock %}
