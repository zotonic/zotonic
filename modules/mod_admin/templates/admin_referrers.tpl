{% extends "admin_base.tpl" %}

{% block title %} admin referrers of {{ m.rsc[q.id].title }} {% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Referrers to “{{ m.rsc[q.id].title }}”</h2>
			
		{% with m.search.paged[{referrers id=q.id page=q.page}] as result %}

			{% ifequal result.total 0 %}
				<p>There are no pages with a connection to the page “{{ m.rsc[q.id].title }}”</p>
			{% else %}
				<p>The following {% ifequal result.total 1 %}page has{% else %}{{ result.total }} pages have{% endifequal %} a connection to the page “{{ m.rsc[q.id].title }}”.</p>
			{% endifequal %}

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}
			
			<h3 class="above-list">Referrers</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-30">Title</span>
					<span class="zp-15">Predicate</span>
					<span class="zp-15">Category</span>
					<span class="zp-15">Modified on</span>
					<span class="zp-15">Modified by</span>
					<span class="zp-10">Options</span>
				</li>
			{% for id, pred_id in result %}
				<li id="{{ #li.id }}" {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-30">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-15">{{ m.rsc[pred_id].title }}</span>
						<span class="zp-15">{{ m.rsc[m.rsc[id].category_id].title }}</span>
						<span class="zp-15">{{ m.rsc[id].modified|date:"d M, H:i" }}</span>
						<span class="zp-15">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
						<span class="zp-10">
							{% button text="delete" disabled=m.rsc[id].is_protected action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
							{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No referrers found
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}

		{% endwith %}

		</div>
	</div>
{% endblock %}
