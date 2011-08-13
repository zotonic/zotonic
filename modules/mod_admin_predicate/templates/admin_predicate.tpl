{% extends "admin_base.tpl" %}

{% block title %} {_ Predicates _} {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Predicates _}</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text=_"make a new predicate" action={dialog_predicate_new title=""} %}
			</div>

			<hr />
			{% endif %}

			<p>{_ A predicate denotes traits or aspects of a page and expresses a relationship between two pages. The relation is always directed, from the subject to the object.<br/>Predicates are defined in ontologies like <a href="http://sioc-project.org/">SIOC</a>.  On this page you can define the predicates known to Zotonic. _}</p>

			<h3 class="above-list">{_ Predicate overview _}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">{_ Title _}</span>
					<span class="zp-20">{_ Name _}</span>
					<span class="zp-40">{_ Uri _}</span>
					<span class="zp-10">{_ Reversed? _}</span>
					<span class="zp-10">{_ Actions _}</span>
				</li>

			{% for name,p in m.predicate %}
				<li id="{{ #li.name }}">
					<a href="{% url admin_edit_rsc id=p.id %}" class="clearfix">
						<span class="zp-20">{{ p.title|default:"&nbsp;" }}</span>
						<span class="zp-20">{{ p.name|default:"&nbsp;" }}</span>
						<span class="zp-40">{{ p.uri|default:"&nbsp;" }}</span>
						<span class="zp-10">{{ p.reversed|yesno:"reversed,&nbsp;" }}</span>
						<span class="zp-10">
							{% button disabled=p.is_protected text=_"delete" action={dialog_predicate_delete id=p.id on_success={slide_fade_out target=#li.name}} %}
							{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=p.id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					{_ No predicates found. _}
				</li>
			{% endfor %}
			</ul>

		</div>
	</div>
{% endwith %}
{% endblock %}
