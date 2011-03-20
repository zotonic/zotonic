{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Valid between _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
{% with m.predicate[id] as p %}
<fieldset class="admin-form">
	<div class="notification notice">
		{_ This predicate can be used between two pages of the following categories. _}<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about predicates. _}', text: '{_ You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to.  When you don\'t check anything then all categories are valid. _}', width: '450px'">{_ Need more help? _}</a>
	</div>

	<div class="form-item clearfix">
		<input id="field-reversed" type="checkbox" class="do_fieldreplace" name="reversed" {% if r.reversed %}checked="checked"{% endif %} value="1" />
		<label for="field-reversed">{_ The direction (from/to) of this predicate is reversed from the normal definition. _}</label>
	</div>

	<div class="zp-30">
		<h4>{_ From category _}</h4>
		<p>
			{% for cat_id, level, indent, title in m.category.all_flat_meta %}
				<label for="{{ #subject.cat_id }}">
					{{ indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="predicate_subject" 
					{% if cat_id|member:p.subject %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
				</label>
			{% endfor %}
		</p>
	</div>

	<div class="zp-20">
		&nbsp;
	</div>

	<div class="zp-30">
		<h4>{_ To category _}</h4>
		<p>
			{% for cat_id, level, indent, title in m.category.all_flat_meta %}
				<label for="{{ #object.cat_id }}">
					{{ indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="predicate_object"  
					{% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
				</label>
			{% endfor %}
		</p>
	</div>

	<div class="zp-20">
		&nbsp;
	</div>

	<hr style="clear:left" />
	{% include "_admin_save_buttons.tpl" %}
</fieldset>
{% endwith %}
{% endblock %}
