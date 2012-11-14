{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}{_ SEO Content _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-seo{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}

<div class="row">
    <div class="control-group span4">
	    <label class="control-label checkbox" for="custom-slug">
            <input id="custom-slug" type="checkbox" name="custom_slug" {% if r.custom_slug %}checked="checked"{% endif %} value="1" onclick="$('#custom-slug:checked').val() ? $('#slug').removeAttr('disabled') : $('#slug').attr('disabled', 'disabled');" title="{_ Customize page slug _}" />
            {_ Page slug _}
        </label>
        <div class="controls">
            <div class="input-prepend">
	            <input type="text" id="slug" name="slug" value="{{ r.slug }}" {% if not r.custom_slug %}disabled="disabled"{% endif %} class="span4" />
            </div>
        </div>
    </div>
    <div class="control-group span4">
	    <label class="control-label" for="seo_title">{_ Page title _}</label>
        <div class="controls">
	        <input type="text" id="seo_title" name="seo_title" class="span4" value="{{ r.seo_title }}"/>
        </div>
    </div>
</div>
    
<div class="row">
    <div class="control-group span4">
	    <label class="control-label" for="title">
            &nbsp;
        </label>
        <div class="controls">
            <label class="checkbox">
                <input id="no-google" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />
	        {_ Ask google to not index this page _}
            </label>
        </div>            
    </div>

    <div class="control-group span4">
	<label class="control-label" for="seo_keywords">{_ Page keywords _}</label>
        <div class="controls">
	    <input type="text" id="seo_keywords" name="seo_keywords" class="span4" value="{{ r.seo_keywords }}"/>
	</div>
    </div>
</div>

<div>
    <label class="control-label" for="seo_desc">{_ Page description _}</label>
    <textarea rows="5" cols="10" id="seo_desc" name="seo_desc" class="seo-desc span8">{{ r.seo_desc }}</textarea>
</div>
{% endwith %}
{% endblock %}
