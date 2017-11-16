{% extends "admin_edit_widget_std.tpl" %}

{# Widget to edit some advanced rsc props #}

{% block widget_title %}
{_ Advanced _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-advanced{% endblock %}

{% block widget_content %}
<fieldset class="form-horizontal">
    {% if id.is_authoritative %}
	    <div class="form-group row">
		    <label class="control-label col-md-3" for="field-page-path">{_ Page path _}</label>
            <div class="col-md-9">
		        <input class="form-control" type="text" id="field-page-path" name="page_path" value="{{ id.page_path }}" {% if not id.is_editable %}disabled="disabled"{% endif %}  {% include "_language_attrs.tpl" language=`en` %} placeholder="{{ id.default_page_url|escape }}" />
            </div>
	    </div>

        <div class="form-group row">
            <div class="col-md-9 col-md-offset-3 checkbox">
                <label class="control-label">
                    <input type="checkbox" id="field-is-page-path-multiple"
                        name="is_page_path_multiple" value="1"
                        {% if id.is_page_path_multiple %}checked{% endif %}
                        {% if not id.is_editable %}disabled="disabled"{% endif %}
                    />
                    {_ Show page on multiple paths _}
                </label>
            </div>
        </div>

	    <div class="form-group row">
	        {% if m.acl.use.mod_admin_config %}
	            <label class="control-label col-md-3" for="field-name">{_ Unique name _}</label>
                <div class="col-md-9">
	                <input class="form-control" type="text" id="name" name="name" value="{{ id.name }}" {% if not id.is_editable or id == 1 %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=`en` %}/>
                </div>
                {% validate id="name" type={name_unique id=id failure_message=_"This name is in use by another page."} %}
	        {% else %}
	            &nbsp;
	        {% endif %}
        </div>

    {% endif %}

    {% if id.installed_by %}
        <div class="alert alert-info edit-message">{_ This resource was created by the module _} <strong>{{ m.modules.info[id.installed_by].title|default:id.installed_by }}</strong></div>
    {% endif %}

    {% if m.acl.use.mod_admin_config %}
	    {% if id.is_a.meta or not id.is_authoritative %}
	        <div class="form-group row">
                <label class="control-label col-md-3" for="field-uri">{_ Unique uri _}</label>
                <div class="col-md-9">
                    <input class="form-control" type="text" id="field-uri" name="uri" value="{{ id.uri }}" {% if not id.is_editable %}disabled="disabled"{% endif %} />
                </div>
            </div>
	    {% endif %}
    {% endif %}

    {% block edit_advanced_extra %}
    {% endblock %}
</fieldset>
{% endblock %}
