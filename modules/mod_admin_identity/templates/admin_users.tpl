{% extends "admin_base.tpl" %}

{% block title %}{_ Users _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>User Overview</h2>
	
		{% if m.acl.is_admin %}
			{% button text=_"Make a new user" action={dialog_user_add on_success={reload}} %}
		{% else %}
			<p>{_ You need to be an administrator to add users. _}</p>
		{% endif %}
		
		<hr class="clear" />

		<div class="clearfix">
			<p>
			 {_ Every page/person can be made into a user on the edit page.
				The difference between a user and a normal page is only
				that the former has logon credentials attached to its page record. _}
			</p>

		</div>

	
	{% with m.acl.user as me %}

		{% with m.search.paged[{users text=q.qs page=q.page}] as result %}

			<h3 class="above-list ">
				{_ Users _}{% if q.qs %}, 
					{_ matching _} “{{ q.qs|escape }}”
					{% button text=_"show all" action={redirect dispatch="admin_user"} %}
				{% else %} {_ overview _}{% endif %}
			</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">{_ Name _}</span>
					<span class="zp-15">{_ Username _}</span>
					<span class="zp-10">{_ Modified on _}</span>
					<span class="zp-10">{_ Created on _}</span>
					<span class="zp-30">{_ Options _}</span>
				</li>
			{% for id, rank in result %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-20">{{ m.rsc[id].title|striptags }}</span>
						<span class="zp-15">{{ m.identity[id].username|escape }}{% if id == me %}  <strong>(that's you)</strong>{% endif %}</span>
						<span class="zp-10">{{ m.rsc[id].modified|date:"d M, H:i" }}</span>
						<span class="zp-10">{{ m.rsc[id].created|date:"d M, H:i" }}</span>
						<span class="zp-30">
							{% button action={dialog_set_username_password id=id} text=_"set username/ password" on_delete={slide_fade_out target=#li.id} %}
							{% if id /= 1 %}
								{% button text=_"delete username" action={dialog_delete_username id=id on_success={slide_fade_out target=#li.id}} %}
							{% endif %}
							{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					{_ No users found. _}
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_user" qargs %}

		{% endwith %}

	{% endwith %}

		</div>
	</div>
{% endblock %}