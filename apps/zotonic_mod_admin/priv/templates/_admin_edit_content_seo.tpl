{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}
{_ SEO Content _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-seo{% endblock %}

{% block widget_content %}
<fieldset class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3" for="custom-slug">
            {_ Page slug _}
        </label>
        <div class="col-md-9">
            <div class="input-group">
                <div class="input-group-addon">
                    <input class="checkbox" id="custom-slug" type="checkbox" name="custom_slug" {% if id.custom_slug %}checked="checked"{% endif %} value="1" onclick="$('#custom-slug:checked').val() ? $('#slug').removeAttr('disabled') : $('#slug').attr('disabled', 'disabled');" title="{_ Customize page slug _}" />
                </div>
                <input type="text" id="slug" name="slug" value="{{ id.slug }}" {% if not id.custom_slug %}disabled="disabled"{% endif %} class="input-xlarge form-control" />
            </div>
        </div>
    </div>

    <div class="form-group row">
        <div class="col-md-9 col-md-offset-3" for="custom-slug">
            <div class="checkbox"><label>
                    <input id="seo_noindex" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if id.seo_noindex %}checked="checked"{% endif %} value="1" />
                    {_ Ask google to not index this page _}
                </label></div>
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="seo_desc">{_ Page description _}</label>
        <div class="col-md-9">
            <textarea rows="5" cols="10" id="seo_desc" name="seo_desc" class="seo-desc form-control">{{ id.seo_desc }}</textarea>
        </div>
    </div>

    {% all catinclude "_admin_edit_content_seo_extra.tpl" id %}
</fieldset>
{% endblock %}
