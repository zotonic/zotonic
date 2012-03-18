{% extends "admin_edit_widget_std.tpl" %}

{# Widget to edit some advanced rsc props #}


{% block widget_title %}{_ Advanced _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
<fieldset>
    {% if m.rsc[id].is_authoritative %}
    
    <div class="row">
        <div class="span4">
	    <div class="control-group">
		<label class="control-label" for="field-page-path">{_ Page path, default is _} <em>{{ r.default_page_url|escape }}</em></label>
                <div class="controls">
		    <input type="text" id="field-page-path" name="page_path" value="{{ r.page_path }}" {% if not is_editable %}disabled="disabled"{% endif %}  {% include "_language_attrs.tpl" language=`en` %}/>
		</div>
	    </div>
        </div>

        <div class="span4">
	    <div class="control-group">
	        {% if m.acl.use.mod_admin_config %}
	        <label class="control-label" for="field-name">{_ Unique name _}</label>
                <div class="controls">
	            <input type="text" id="field-name" name="name" value="{{ r.name }}" {% if not is_editable or id == 1 %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=`en` %}/>
                </div>
	        {% else %}
	        &nbsp;
	        {% endif %}
            </div>
	</div>
    </div>
    {% endif %}

    {% if r.installed_by %}
    <div class="alert alert-info">{_ This resource was created by the module _} <strong>{{ m.modules.info[r.installed_by].title|default:r.installed_by }}</strong></div>
    {% endif %}

    {% if m.acl.use.mod_admin_config %}
	{% if r.is_a.meta or not r.is_authoritative %}
	    <div class="form-item clearfix">
		<label for="field-name">{_ Unique uri _}</label>
		<input type="text" id="field-name" name="uri" value="{{ r.uri }}" {% if not is_editable %}disabled="disabled"{% endif %} />
	    </div>
	{% endif %}
    {% endif %}

</fieldset>

{% endblock %}
