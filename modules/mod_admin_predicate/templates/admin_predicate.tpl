{% extends "admin_base.tpl" %}

{% block title %} Predicates {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Predicates</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text="make a new predicate" action={dialog_predicate_new title=""} %}
			</div>

			<hr />
			{% endif %}

			<p>A predicate denotes traits or aspects of a page and expresses a relationship between two pages.
			 The relation is always directed, from the subject to the object.<br/>Predicates are defined in ontologies like <a href="http://sioc-project.org/">SIOC</a>.  On this page you can define the predicates known to Zotonic.</p>
			
			<h3 class="above-list">Predicate overview</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-20">Name</span>
					<span class="zp-40">Uri</span>
					<span class="zp-10">Reversed?</span>
					<span class="zp-10">Actions</span>
				</li>

			{% for name,p in m.predicate %}
				<li id="{{ #li.name }}">
					<a href="{% url admin_edit_rsc id=p.id %}" class="clearfix">
						<span class="zp-20">{{ p.title|default:"&nbsp;" }}</span>
						<span class="zp-20">{{ p.name|default:"&nbsp;" }}</span>
						<span class="zp-40">{{ p.uri|default:"&nbsp;" }}</span>
						<span class="zp-10">{{ p.reversed|yesno:"reversed,&nbsp;" }}</span>
						<span class="zp-10">
							{% button disabled=p.is_protected text="delete" action={dialog_predicate_delete id=p.id on_success={slide_fade_out target=#li.name}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=p.id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No predicates found.
				</li>
			{% endfor %}
			</ul>

		</div>
	</div>
{% endwith %}
{% endblock %}