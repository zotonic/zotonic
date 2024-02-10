{% extends "admin_edit_widget_std.tpl" %}

{# Widget with main rsc controls: publish, delete, duplicate, etc  #}

{% block widget_title %}
    {_ Publish this page _}
    <div class="widget-header-tools"></div>
{% endblock %}

{% block show_opened %}true{% endblock %}
{% block widget_id %}sidebar-publish-page{% endblock %}
{% block widget_class %}dark{% endblock %}


{% block widget_content %}
<div class="form-group">
    {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
    {% if id.page_url as page_url %}
        {% if id.is_editable %}
            {% button type="submit" id="save_view" class="btn btn-outline-dark" text=_"Save and View" title=_"Save and view the page." %}
        {% endif %}

        {% include "_admin_edit_content_publish_view.tpl" %}
    {% endif %}

    <div class="pull-right">
        <div class="btn-group">
            {% with m.rsc[q.qcat].id|default:id.category_id as cat_id %}
                <a id="rsc-prev-incat" href="#previous" class="btn btn-outline-dark btn-sm"
                   title="{_ Previous in category: _} {{ cat_id.title }}">
                    <span class='glyphicon glyphicon-arrow-left'></span>
                </a>
                <a id="rsc-next-incat" href="#previous" class="btn btn-outline-dark btn-sm"
                   title="{_ Next in category: _} {{ cat_id.title }}">
                    <span class='glyphicon glyphicon-arrow-right'></span>
                </a>
                {% wire id="rsc-prev-incat" action={redirect_incat id=id cat_id=cat_id is_prev} %}
                {% wire id="rsc-next-incat" action={redirect_incat id=id cat_id=cat_id is_next} %}
            {% endwith %}
        </div>
    </div>
</div>

<div class="d-flex">
    <div class="form-check me-3">
        <input type="checkbox" id="is_published" class="form-check-input" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
        <label for="is_published" class="form-check-label" style="padding-left: 0">
            {_ Published _}
        </label>
    </div>
    <div class="form-check">
        <input type="checkbox" id="is_featured" class="form-check-input" name="is_featured" value="1" {% if id.is_featured %}checked="checked"{% endif %}/>
        <label for="is_featured" class="form-check-label">
            {_ Featured _}
        </label>
    </div>
</div>
{% endblock %}
