{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing rsc category #}

{% block widget_title %}{_ Category _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
{% with m.rsc[id] as r %}

{# meta categories (predicate, category and group) cannot be changed #}
{% if not r.is_a.meta %}
<div class="notification notice">
	{_ The category defines what the page represents. _} 
	<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{{ _"Help about category."|escapejs }}', text: '{{ _"Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle."|escapejs }}', width: '450px'">{_ Need more help? _}</a>
</div>

<p>
    {% with r.category_id as r_cat %}
	<select id="category_id" name="category_id">
	    {% for cat_id, level, indent, name in m.category.all_flat %}
		{% if m.acl.insert[name|as_atom] %}
		    <option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
			{{ indent }}{{ m.rsc[cat_id].title|default:name }}
		    </option>
		{% endif %}
	    {% endfor %}
	</select>
    {% endwith %}
</p>

<div class="form-item clearfix">
	{% button   type="submit" 
		id="save_stay" 
		class="save-resource do_tooltip" 
		text=_"Save this page" 
		title=_"Save this page and change category." 
		disabled=not is_editable %}

	{% button class="discard-resource" text=_"Cancel" action={redirect back} %}
</div>

{% else %}
<div class="notification notice">
	{_ This page is a _} {{ m.rsc[r.category_id].title }}.
	{_ Predicates, groups and categories can't be changed into another category. _}
</div>
{% endif %}

{% endwith %}
{% endblock %}
