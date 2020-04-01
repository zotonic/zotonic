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
    <div class="pull-right">
        {% button class="btn btn-default" text=_"Cancel" action={redirect back} title=_"Go back." tag="a" %}
    </div>
    {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not is_editable %}
    {% if id.page_url as page_url %}
        {% if is_editable %}
            {% button type="submit" id="save_view" class="btn btn-default" text=_"Save and View" title=_"Save and view the page." %}
        {% endif %}

        {% include "_admin_edit_content_publish_view.tpl" %}
    {% endif %}
</div>

<div class="form-group">
    <label for="is_published" class="checkbox-inline">
        <input type="checkbox" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/>
        {_ Published _}
    </label>

    <label for="is_featured" class="checkbox-inline">
        <input type="checkbox" id="is_featured" name="is_featured" value="1" {% if r.is_featured %}checked="checked"{% endif %}/>
        {_ Featured _}
    </label>

    <label for="is_protected" class="checkbox-inline" title="{_ Protect from deletion _}">
        <input type="checkbox" id="is_protected" name="is_protected" value="1" {% if r.is_protected %}checked="checked"{% endif %} {% if id == 1 %}disabled="disabled"{% endif %} />
        {_ Protect _}
    </label>

    <label for="is_dependent" class="checkbox-inline" title="{_ Delete if no other page is connected to this page. _}">
        <input type="checkbox" id="is_dependent" name="is_dependent" value="1" {% if r.is_dependent %}checked="checked"{% endif %} {% if id == 1 or id.is_protected %}disabled="disabled"{% endif %} />
        {_ Dependent _}
    </label>
</div>

<div class="form-group">
    <div class="pull-right">
        <div class="btn-group">
            {% with m.rsc[q.qcat].id|default:id.category_id as cat_id %}
                <a id="rsc-prev-incat" href="#previous" class="btn btn-default btn-sm"
                   title="{_ Previous in category: _} {{ cat_id.title }}">
                    <span class='glyphicon glyphicon-arrow-left'></span>
                </a>
                <a id="rsc-next-incat" href="#previous" class="btn btn-default btn-sm"
                   title="{_ Next in category: _} {{ cat_id.title }}">
                    <span class='glyphicon glyphicon-arrow-right'></span>
                </a>
                {% wire id="rsc-prev-incat" action={redirect_incat id=id cat_id=cat_id is_prev} %}
                {% wire id="rsc-next-incat" action={redirect_incat id=id cat_id=cat_id is_next} %}
            {% endwith %}
        </div>
    </div>

    {% ifnotequal id 1 %}
        {% button class="btn btn-default btn-sm" disabled=(r.is_protected or not m.rsc[id].is_deletable) id="delete-button" text=_"Delete" action={dialog_delete_rsc id=r.id on_success={redirect back}} title=_"Delete this page." %}
    {% endifnotequal %}

    {% if is_editable %}
        {% button type="submit" id="save_duplicate" class="btn btn-default btn-sm" text=_"Duplicate" title=_"Duplicate this page." %}
    {% else %}
        {% button class="btn btn-default btn-sm"
            text=_"Duplicate"
            action={dialog_duplicate_rsc id=id}
            title=_"Duplicate this page."
            disabled=(not m.acl.insert[r.category.name]) %}
    {% endif %}
</div>
{% endblock %}
