{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editin  media/file website-related props #}

{% block widget_title %}{_ Website _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
<fieldset class="admin-form">
	<div class="form-item clearfix">
		<label for="media-website">{_ Website for clicks on image _}</label>
		<input type="text" id="media-website" name="website" class="span8" value="{{ r.website }}"/>
	</div>
</fieldset>
{% endblock %}
