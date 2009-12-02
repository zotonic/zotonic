{% extends "admin_base.tpl" %}

{% block title %}Mailing Lists{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Mailing lists</h2>

		{% button text="New mailing list" action={dialog_new_rsc cat="mailinglist"} %}

		<hr class="clear" />
		<p>
			Any page can be send as a mailing. You can send a mailing from any edit page, here you can add or remove mailing lists and recipients.
		</p>

		<h3 class="above-list ">Mailing list overview</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">Title</span>
				<span class="zp-40">Description</span>
				<span class="zp-10">Recipients</span>
				<span class="zp-10">Scheduled</span>
				<span class="zp-20">Actions</span>
			</li>
		{% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
			<li id="mailinglist-{{id}}">
				{% with m.rsc[id].is_editable as enabled %}
				<a href="{% url admin_mailinglist_recipients id=id %}" class="clearfix">
					<span class="zp-20">{{ title|default:"untitled" }}</span>
					<span class="zp-40">{{ m.rsc[id].summary|default:"-" }}</span>
					{% with m.mailinglist.stats[id] as stats %}
						<span class="zp-10">{{ stats[1]|format_number }}</span>
						<span class="zp-10">{{ stats[2]|length|format_number }}</span>
					{% endwith %}
					<span class="zp-20">
						{% button text="recipients" action={redirect dispatch="admin_mailinglist_recipients" id=id} disabled=enabled|not %}
						{% if enabled %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						{% else %}
							{% button text="view" action={redirect dispatch="admin_edit_rsc" id=id} %}
						{% endif %}
						{% button text="delete" postback={mailinglist_delete_confirm id=id} disabled=enabled|not %}
					</span>
				</a>
				{% endwith %}
			</li>
		{% empty %}
			<li>
				No items found
			</li>
		{% endfor %}
		</ul>

		</div>
	</div>
{% endblock %}