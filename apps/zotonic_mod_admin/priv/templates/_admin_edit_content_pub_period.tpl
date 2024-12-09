{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing rsc publication date_start/date_end #}

{% block widget_title %}
{_ Publication period _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Publication date range",
                text: _"If a page has a publication date range then it will only be visible between the two dates. Note that if you are allowed to edit the page then you can always see it."
            }|escape
        }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}{{ id.publication_start|in_past and id.publication_end|in_future }}{% endblock %}
{% block widget_id %}sidebar-pub-period{% endblock %}

{% block widget_content %}
<p class="help-block" {% if not id.tz or id.tz == m.req.timezone %}style="display:none"{% endif %}>
    <i class="fa fa-exclamation-triangle"></i>
    {_ Showing dates in _}: <b class="rsc-timezone">{{ id.tz|escape }}</b>
</p>
<fieldset>
    <div class="row">
        <div class="col-sm-6">
            <div class="form-group">
                <label class="control-label">{_ Visible from _}</label>
                <div>
                    {% include "_edit_date.tpl" date=id.publication_start name="publication_start" is_end=0 is_editable=id.is_editable timezone=id.tz %}
                </div>
            </div>
        </div>
        <div class="col-sm-6">
            <div class="form-group">
                <label class="control-label">{_ Visible till _}</label>
                <div>
                    {% include "_edit_date.tpl" date=id.publication_end name="publication_end" is_end=1 is_editable=id.is_editable timezone=id.tz %}
                </div>
            </div>
        </div>
    </div>

    <div class="form-group">
        <label class="control-label">{_ Publication date of original article _}</label>
        <div>
            {% include "_edit_date.tpl" date=id.org_pubdate name="org_pubdate" is_end=0 is_editable=id.is_editable timezone=id.tz %}
        </div>
    </div>
</fieldset>
{% endblock %}
