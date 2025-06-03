{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget to edit some advanced rsc props #}

{% block widget_title %}
{_ Advanced _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-advanced{% endblock %}

{% block widget_content %}
    {% if m.acl.use.mod_admin %}
        {% with "field-page-path" ++ lang_code_for_id as elt_id %}
        {% with "page_path" ++ lang_code_with_dollar as elt_name %}
        <div class="form-group label-floating">
            <input type="text" id="{{ elt_id }}"
                name="{{ elt_name }}"
                placeholder="{_ Page path _} {{ lang_code_with_brackets }} &mdash; {% with lang_code as z_language %}
{{ id.default_page_url|escape }}{% endwith %}"
                value="{{ (is_i18n|if : id.translation[lang_code].page_path : id.page_path)|urldecode|escape }}"
                {% if not id.is_editable %}disabled="disabled"{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="form-control" %}>
            <label class="control-label" for="{{ elt_id }}">
                {_ Page path _} {{ lang_code_with_brackets }}
            </label>
            {% validate id=elt_id name=elt_name
                        type={page_path_unique id=id failure_message=_"This page path is in use by another page."}
            %}
            <p class="help-block">
                {% if m.translation.default_language == lang_code %}
                    {_ The path <em>after the language code</em> for the URL, if left empty then the path of one of the other languages is used. _}
                {% else %}
                    {_ The path <em>after the language code</em> for the URL, if left empty then the path of the default language is used. _}
                {% endif %}
                <br>
                {% with lang_code as z_language %}
                    {_ If none of the languages has a page path then the complete path will be: _}
                    <br><tt>{{ id.default_page_url|escape }}</tt>
                {% endwith %}
            </p>
        </div>
        {% endwith %}
        {% endwith %}
    {% elseif id.page_path %}
        <label class="control-label">{_ Page path _} {{ lang_code_with_brackets }}</label>
        <p><b>{{ id.page_path }}</b></p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="form-group">
        <label class="control-label">
            <input type="checkbox" id="field-is-page-path-multiple"
                name="is_page_path_multiple" value="1"
                {% if id.is_page_path_multiple %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %}>
            {_ Show page on multiple paths _}
        </label>
        <p class="help-block">{_ If not checked then a visitor is always redirected to the canonical URL of this page. _}</p>
    </div>

    <div class="form-group">
        <label class="control-label">
            <input type="checkbox" id="field-is-unfindable"
                name="is_unfindable" value="1"
                {% if id.is_unfindable %}checked{% endif %}
                {% if not id.is_editable %}disabled="disabled"{% endif %}>
            {_ Hide page from searches (depends on the search query) _}
        </label>
    </div>

    <div class="form-group label-floating">
        {% if m.acl.use.mod_admin %}
            <input class="form-control" type="text" id="name" name="name" value="{{ id.name }}"
                   {% if not id.is_editable or id == 1 %}disabled="disabled"{% endif %}
                   {% include "_language_attrs.tpl" language=`en` %}
                   placeholder="{_ Unique name _}">
            <label class="control-label" for="field-name">{_ Unique name _}</label>
            {% validate id="name" type={name_unique id=id failure_message=_"This name is in use by another page."} %}
        {% elseif id.name %}
            <label class="control-label">{_ Unique name _}</label>
            <p><b>{{ id.name }}</b></p>
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
