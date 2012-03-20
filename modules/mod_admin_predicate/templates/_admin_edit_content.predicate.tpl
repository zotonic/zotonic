{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Valid between _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}

<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-mini btn-primary do_dialog" data-dialog="title: '{_ Help about predicates. _}', text: '{_ You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to.  When you don\'t check anything then all categories are valid. _}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>

{% with m.predicate[id] as p %}

<fieldset>

    <p>{_ This predicate can be used between two pages of the following categories. _}</p>
    
    <div class="row">

        <div class="span4">
	    <h4>{_ From category _}</h4>

	    {% for cat_id, level, indent, title in m.category.all_flat_meta %}
	    <label class="checkbox">
		{{ indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="predicate_subject" 
		{% if cat_id|member:p.subject %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}
	    </label>
	    {% endfor %}
	</div>

	<div class="span4">
	    <h4>{_ To category _}</h4>

	    {% for cat_id, level, indent, title in m.category.all_flat_meta %}
	    <label class="checkbox">
		{{ indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="predicate_object"  
		{% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
	    </label>
	    {% endfor %}
	</div>
    </div>

    <hr />
    <div class="control-group">
        <div class="controls">
	    <label class="inline checkbox">
                <input id="field-reversed" type="checkbox" class="do_fieldreplace" name="reversed" {% if r.reversed %}checked="checked"{% endif %} value="1" />
		{_ The direction (from/to) of this predicate is reversed from the normal definition. _}
            </label>
	</div>
    </div>

    
    {% include "_admin_save_buttons.tpl" %}
</fieldset>
{% endwith %}
{% endblock %}
