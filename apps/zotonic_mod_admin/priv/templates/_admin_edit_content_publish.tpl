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
<div class="form-group buttons">
    {% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
    {% if id.page_url as page_url %}
        {% if id.is_editable %}
            {% button type="submit" id="save_view" class="btn btn-default" text=_"Save and View" title=_"Save and view the page." %}
        {% endif %}

        {% include "_admin_edit_content_publish_view.tpl" %}
    {% endif %}

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
</div>

<div class="form-group">
    <label for="is_published" class="checkbox-inline" style="padding-left: 0">
        <input type="checkbox" id="is_published" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
        {_ Published _}
    </label>

    <label for="is_featured" class="checkbox-inline">
        <input type="checkbox" id="is_featured" name="is_featured" value="1" {% if id.is_featured %}checked="checked"{% endif %}/>
        {_ Featured _}
    </label>

    <div class="pull-right">
        <div class="btn-group">
            <button type="button" class="btn btn-default btn-sm dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" aria-label="{_ More actions: duplicate or delete page _}">
                {_ More _} <span class="caret"></span>
            </button>
            <ul class="dropdown-menu dropdown-menu-right">
                {% block more_actions %}
                    {% if m.acl.insert[id.category.name] %}
                        <li>
                            <a href="#duplicate" id="duplicate-rsc-action" role="menuitem" aria-label="{_ Make a duplicate of this page. _}">
                                <span class="glyphicon glyphicon-duplicate"></span> {_ Duplicate this page... _}
                            </a>
                            {% wire id="duplicate-rsc-action" action={dialog_duplicate_rsc id=id} %}
                        </li>
                    {% endif %}
                    {% if id.is_deletable %}
                        <li role="separator" class="divider"></li>
                        <li>
                            <a href="#delete" id="delete-rsc-action" role="menuitem" aria-label="{_ Delete this page. _}">
                                <span class="glyphicon glyphicon-trash"></span> {_ Delete this page... _}
                            </a>
                        </li>
                        {% wire id="delete-rsc-action" action={dialog_delete_rsc id=id on_success={redirect back}} %}
                    {% endif %}
                {% endblock %}
            </ul>
        </div>
    </div>
</div>
{% endblock %}
