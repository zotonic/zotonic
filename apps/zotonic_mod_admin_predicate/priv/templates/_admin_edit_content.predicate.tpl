{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Valid between _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Help about predicates",
                text: _"You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to. When you don’t check anything then all categories are valid."
            }|escape
        }}, " title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
{% with m.predicate[id] as p %}
<fieldset>
    <p>
        {_ This predicate can be used between two pages of the following categories. _}
        {_ This is used to filter the predicates in the <i>Page connections</i> panel and the pages in the connect dialogs. _}
    </p>

    <div class="row">

        <div class="col-lg-6 col-md-6">
            <h4>{_ From category _}</h4>

            {% for c in m.category.tree_flat_meta %}
                {% with c.id as cat_id %}
                    <div class="checkbox">
                        <label>
                            {{ c.indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="predicate_subject_list[]"
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
                            {{ c.indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="predicate_object_list[]"
                            {% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ cat_id.title }}<br/>
                        </label>
                    </div>
                {% endwith %}
            {% endfor %}
        </div>
    </div>

    <hr />

    <h4>{_ Options _}</h4>

    <div class="form-group">
        <label class="checkbox-inline">
            <input type="checkbox" name="is_connect_checkbox" {% if id.is_connect_checkbox %}checked="checked"{% endif %} value="1">
            {_ Use a list of checkboxes with titles when adding page connections. _}
        </label>
        <div class="help-block">
            {_ Instead of a search box this will show a list of all pages in the <i>To</i> categories checked above. At least one category must be checked. _}
        </div>
    </div>

    <div class="form-group">
        <label class="checkbox-inline">
            <input type="checkbox" name="is_insert_before" {% if r.is_insert_before %}checked="checked"{% endif %} value="1">
            {_ Insert new connections before existing connections. _}
        </label>
        <div class="help-block">
            {_ Usually new connections are inserted after existing connections. Check this to insert new connections before existing connections. _}
        </div>
    </div>

    <div class="form-group">
        <label class="checkbox-inline">
            <input type="checkbox" name="is_object_noindex" {% if id.is_object_noindex %}checked="checked"{% endif %} value="1">
            {_ Do not find subjects using this predicate’s object titles. _}
        </label>
        <div class="help-block">
            {_ This directs the indexers to not include the titles of connected pages with this predicate. Normally titles of connected pages are included, making it possible to eg. find articles by their author’s name. _}
        </div>
    </div>

    <div class="form-group">
        <label class="checkbox-inline">
            <input type="checkbox" name="is_connections_hide" {% if id.is_connections_hide %}checked="checked"{% endif %} value="1">
            {_ Do not show on the connections list on the resource edit page. _}
        </label>
        <div class="help-block">
            {_ Some predicates are mostly for internal purposes. Check this if you don’t want to show this predicate on the <em>Connected to</em> list. _}
        </div>
    </div>

    <div class="form-group">
        <label class="checkbox-inline">
            <input type="checkbox" name="reversed" {% if id.reversed %}checked="checked"{% endif %} value="1">
            {_ The direction (from/to) of this predicate is reversed from the normal definition. _}
        </label>
        <div class="help-block">
            {_ Ontologies define predicates for a defined set of subject and object categories. Sometimes it is more practical to have the predicate’s connection direction reversed from the formal definition. Export and import functions can use this option to swap subject/object relation when exporting or importing data. _}
        </div>
    </div>
</fieldset>
{% endwith %}
{% endblock %}
