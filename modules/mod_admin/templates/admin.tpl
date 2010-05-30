{% extends "admin_base.tpl" %}

{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Dashboard _}</h2>

			<div class="clearfix">
				{% all include "_admin_make_page_buttons.tpl" %}
				{% button class="" text=_"Make a new page" action={dialog_new_rsc title=""} %}
				{% button class=""
						text=_"Make a new media item"
						action={dialog_media_upload title=""}
						disabled=(not m.acl.insert.media and not m.acl.insert.image) %}
			</div>

			<hr class="clear" />
			
			<div class="zp-50">

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">
							{_ Latest modified texts _}
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="text"} text=_"show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">{_ Title _}</span>
								<span class="zp-25">{_ Category _}</span>
								<span class="zp-20">{_ Options _}</span>
							</li>
							
							{% for id in m.search[{latest cat="text" pagelen="5"}] %}
							{% if m.rsc[id].is_visible %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text=_"view" action={redirect id=id} %}
										{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% endif %}
							{% empty %}
							<li>
								{_ No articles. _}
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">
							{_ Latest modified persons _}
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="person"} text=_"show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">{_ Title _}</span>
								<span class="zp-25">{_ Category _}</span>
								<span class="zp-20">{_ Options _}</span>
							</li>
							
							{% for id in m.search[{latest cat="person" pagelen="5"}] %}
							{% if m.rsc[id].is_visible %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text=_"view" action={redirect id=id} %}
										{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% endif %}
							{% empty %}
							<li>
								{_ No persons. _}
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

			</div>

			<div class="zp-50">

                {% if m.rsc['location'].id %}
				<div class="padding last">
					<div id="dashboard-pages">
						<h3 class="above-list">
							{_ Latest modified locations _}
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="location"} text=_"show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">{_ Title _}</span>
								<span class="zp-25">{_ Category _}</span>
								<span class="zp-20">{_ Options _}</span>
							</li>
							
							{% for id in m.search[{latest cat="location" pagelen="5"}] %}
							{% if m.rsc[id].is_visible %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text=_"view" action={redirect id=id} %}
										{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% endif %}
							{% empty %}
							<li>
								{_ No locations. _}
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>
                {% endif %}

                {% if m.rsc['event'].id %}
				<div class="padding last">
					<div id="dashboard-pages">
						<h3 class="above-list">
							{_ Latest modified events _}
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="event"} text=_"show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">{_ Title _}</span>
								<span class="zp-25">{_ Start date _}</span>
								<span class="zp-20">{_ Options _}</span>
							</li>
							
							{% for id in m.search[{latest cat="event" pagelen="5"}] %}
							{% if m.rsc[id].is_visible %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].date_start|date:"d M Y, H:i"|default:"-" }}</span>
									<span class="zp-20">
										{% button text=_"view" action={redirect id=id} %}
										{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% endif %}
							{% empty %}
							<li>
								{_ No events. _}
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>
                {% endif %}

				<div class="padding last">
					<div id="dashboard-media">
						<h3 class="above-list">
							{_ Latest modified media items _}
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="media"} text=_"show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-15">{_ Preview _}</span>
								<span class="zp-40">{_ Title _}</span>
								<span class="zp-25">{_ Category _}</span>
								<span class="zp-20">{_ Options _}</span>
							</li>
							
							{% for id in m.search[{latest cat="media" pagelen="5"}] %}
							{% if m.rsc[id].is_visible %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-15">{% image id width=40 height=18 crop %}&nbsp;</span>
									<span class="zp-40">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text=_"view" action={redirect id=id} %}
										{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% endif %}
							{% empty %}
							<li>
								{_ No media found. _}
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

			</div>

		</div>
		<div class="push"></div>
	</div>
{% endblock %}
