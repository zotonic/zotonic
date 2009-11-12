{% extends "admin_base.tpl" %}

{% block title %} admin {% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Zotonic Dashboard</h2>

			<div class="clearfix">
				{% all include "_admin_make_page_buttons.tpl" %}
				{% button class="" text="Make a new page" action={dialog_new_rsc title=""} %}
				{% button class="" text="Make a new media item" action={dialog_media_upload title=""} %}
			</div>

			<hr class="clear" />
			
			<div class="zp-50">

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">
							Latest modified texts
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="text"} text="show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="text" pagelen="5"}] %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">
							Latest modified persons
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="person"} text="show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="person" pagelen="5"}] %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

			</div>

			<div class="zp-50">

				<div class="padding last">
					<div id="dashboard-pages">
						<h3 class="above-list">
							Latest modified locations
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="location"} text="show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="location" pagelen="5"}] %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding last">
					<div id="dashboard-pages">
						<h3 class="above-list">
							Latest modified events
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="event"} text="show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-55">Title</span>
								<span class="zp-25">Start date</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="event" pagelen="5"}] %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].date_start|date:"d M Y, H:i"|default:"-" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding last">
					<div id="dashboard-media">
						<h3 class="above-list">
							Latest modified media items
							{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="media"} text="show all"%}
						</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-15">Preview</span>
								<span class="zp-40">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="media" pagelen="5"}] %}
							<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-15">{% image id width=40 height=18 crop %}&nbsp;</span>
									<span class="zp-40">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No media found.
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
