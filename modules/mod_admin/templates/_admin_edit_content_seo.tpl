{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}{_ SEO Content _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="admin-form">
	<div class="form-item clearfix">
		<input id="no-google" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />
		<label for="no-google">{_ Ask google to not index this page _}</label>
	</div>

	<div class="form-item clearfix">
		<label for="seo_title">{_ Page title _}</label>
		<input type="text" id="seo_title" name="seo_title" class="zp-100" value="{{ r.seo_title }}"/>
	</div>

	<div class="form-item clearfix">
		<label for="title">{_ Page slug _}</label>
		<input type="text" id="slug" name="slug" class="zp-100" value="{{ r.slug }}" {% if not r.custom_slug %}disabled="disabled"{% endif %} />
		<input id="custom-slug" type="checkbox" class="do_fieldreplace" name="custom_slug" {% if r.custom_slug %}checked="checked"{% endif %} 
		value="1" onclick="$('#slug').attr('disabled', $('#custom-slug:checked').val() ? '' : 'disabled');" />
		<label for="custom-slug">{_ Customize page slug _}</label>
	</div>

	<div class="form-item clearfix">
		<label for="seo_keywords">{_ Page keywords _}</label>
		<input type="text" id="seo_keywords" name="seo_keywords" class="zp-100" value="{{ r.seo_keywords }}"/>
	</div>

	<div class="form-item clearfix">
		<label for="seo_desc">{_ Page description _}</label>
		<textarea rows="2" cols="10" id="seo_desc" name="seo_desc" class="seo-desc zp-100">{{ r.seo_desc }}</textarea>
	</div>

	{% include "_admin_save_buttons.tpl" %}
</fieldset>
{% endwith %}
{% endblock %}
