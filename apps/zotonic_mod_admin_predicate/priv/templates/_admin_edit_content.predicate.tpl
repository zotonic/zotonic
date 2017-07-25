{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Valid between _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Help about predicates"|escapejs }}', text: '{{ _"You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to. When you don't check anything then all categories are valid."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
{% with m.predicate[id] as p %}
<fieldset>
    <p>{_ This predicate can be used between two pages of the following categories. _}</p>

    <div class="row">

        <div class="col-lg-6 col-md-6">
            <h4>{_ From category _}</h4>

            {% for c in m.category.tree_flat_meta %}
                {% with c.id as cat_id %}
                    <div class="checkbox">
                        <label>
                            {{ c.indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="predicate_subject"
                            {% if cat_id|member:p.subject %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ cat_id.title }}
                        </label>
                    </div>
                {% endwith %}
            {% endfor %}
        </div>

        <div class="col-lg-6 col-md-6">
            <h4>{_ To category _}</h4>

            {% for c in m.category.tree_flat_meta %}
                {% with c.id as cat_id %}
                    <div class="checkbox">
                        <label>
                            {{ c.indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="predicate_object"
                            {% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ cat_id.title }}<br/>
                        </label>
                    </div>
                {% endwith %}
            {% endfor %}
        </div>
    </div>

    <hr />

    <div class="form-group">
        <label class="checkbox-inline">
            <input id="field-reversed" type="checkbox" class="do_fieldreplace" name="reversed" {% if r.reversed %}checked="checked"{% endif %} value="1" />{_ The direction (from/to) of this predicate is reversed from the normal definition. _}
        </label>
    </div>

    <div class="form-group">
        <label class="checkbox-inline">
            <input id="field-reversed" type="checkbox" class="do_fieldreplace" name="is_object_noindex" {% if r.is_object_noindex %}checked="checked"{% endif %} value="1" />{_ Do not find subjects using this predicate’s object titles. _}
        </label>
    </div>
</fieldset>
{% endwith %}
{% endblock %}
