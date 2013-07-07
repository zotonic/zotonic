{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ m.rsc[id].title }}”{% endblock %}


{% block content %}
{% with m.rsc[id] as r %}
{% with r.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}

<div class="edit-header">
	<div class="pull-right">
		<p class="admin-chapeau">
			{_ Modified _} {_ by _}
			<a href="{% url admin_edit_rsc id=r.modifier_id %}">{{ m.rsc[r.modifier_id].title }}</a> &ndash; {{ r.modified|date:"Y-m-d H:i" }}<br/>
			{_ Created by _}
			<a href="{% url admin_edit_rsc id=r.creator_id %}">{{ m.rsc[r.creator_id].title }}</a> &ndash; {{ r.created|date:"Y-m-d H:i" }}
		</p>
	</div>

	{% if not is_editable %}
	<h2>
	{_ You are not allowed to edit the _} {{ m.rsc[r.category_id].title|lower }} “<span {% include "_language_attrs.tpl" %}>{{ r.title|striptags }}</span>”
	</h2>
	{% else %}
	<p class="admin-chapeau">
		{_ editing _}
		{% if m.acl.insert[r.category.name|as_atom] and not r.is_a.category and not r.is_a.predicate %}
		<a	href="javascript:;" id="changecategory" title="{_ Change category _}">{{ m.rsc[r.category_id].title|lower }}</a>:
		{% wire id="changecategory" action={dialog_open title=_"Change category" template="_action_dialog_change_category.tpl" id=id} %}
		{% else %}
		{{ m.rsc[r.category_id].title|lower }}:
		{% endif %}
	</p>
	
	<h2 {% include "_language_attrs.tpl" %}>
		{{ r.title|striptags|default:_"<em>untitled</em>" }}
	</h2>
	{% endif %}{# editable #}
</div>

{% block admin_edit_form_pre %}{% endblock %}

{% wire id="rscform" type="submit" postback="rscform" %}
<form id="rscform" method="post" action="postback" class="form-horizontal">
	<input type="hidden" name="id" value="{{ id }}" />

	<div class="row-fluid">
		<div class="span8" id="poststuff">
			{% catinclude "_admin_edit_main_parts.tpl" id is_editable=is_editable languages=languages r=r %}
		</div>

		<div class="span4" id="sidebar">
			{% catinclude "_admin_edit_sidebar_parts.tpl" id is_editable=is_editable languages=languages r=r %}
		</div>
	</div>
</form>

{% block admin_edit_form_post %}{% endblock %}

</div>

{% endwith %}
{% endwith %}
{% endwith %}

{% include "_admin_edit_js.tpl" %}

{% endblock %}

{% block tinymce %}
	{% include "_admin_tinymce.tpl" %}
{% endblock %}

