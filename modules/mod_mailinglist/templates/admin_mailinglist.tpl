{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing Lists _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>{_ Mailing lists _}</h2>

		{% button text=_"New mailing list" action={dialog_new_rsc cat="mailinglist"} %}

		<hr class="clear" />
		<p>{_ Any page can be sent as a mailing. You can send a mailing from any edit page. On this page you can add or remove mailing lists and their recipients. _}<br/>
		{_ Recipients are subscribed either as email-only (via a simple signup form), or as subscribed persons in the system. _}</p>

		<h3 class="above-list ">{_ Mailing list overview _}</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">{_ Title _}</span>
				<span class="zp-40">{_ Description _}</span>
				<span class="zp-10">{_ Recipients _}</span>
				<span class="zp-10">{_ Scheduled _}</span>
				<span class="zp-20">{_ Actions _}</span>
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
						{% button text=_"recipients" action={redirect dispatch="admin_mailinglist_recipients" id=id} disabled=not enabled %}
						{% if enabled %}
							{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						{% else %}
							{% button text=_"view" action={redirect dispatch="admin_edit_rsc" id=id} %}
						{% endif %}
						{% button text=_"delete" postback={mailinglist_delete_confirm id=id} disabled=not enabled %}
					</span>
				</a>
				{% endwith %}
			</li>
		{% empty %}
			<li>
				{_ No items found _}
			</li>
		{% endfor %}
		</ul>

		</div>
	</div>
{% endblock %}
