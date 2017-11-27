{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file website-related props #}

{% block widget_title %}
{_ Website _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
<fieldset>
	<div class="form-group">
		<label for="media-website">{_ Website for clicks on image _}</label>
		<input class="form-control" type="text" id="media-website" name="website" value="{{ id.website }}"/>
	</div>

    <div class="form-group">
	    <div class="checkbox">
            <label>
                <input type="checkbox" id="field-is-website=redirect" name="is_website_redirect" value="1"
                    {% if id.is_website_redirect %}checked{% endif %}
                    {% if not id.is_editable %}disabled="disabled"{% endif %}
                />
                {_ Redirect to above website on page view _}
            </label>
        </div>
    </div>
</fieldset>
{% endblock %}
