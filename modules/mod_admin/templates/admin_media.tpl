{% extends "admin_base.tpl" %}

{% block title %} Admin Media {% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Zotonic Media</h2>

			<div class="clearfix">
				{% button
						text="make a new media item" 
						action={dialog_media_upload}
				%}
			</div>

			<hr />
			
			<p>Media encompasses all uploaded images, movies and documents. Media can be attached to pages.</p>
			
			{% with m.search.paged[{fulltext cat="media" text=q.qs page=q.page}] as result %}

				{% pager result=result dispatch="admin_media" qargs %}
				<h3 class="above-list">Media overview</h3>

				<ul class="media-list short-list">
					<li class="headers clearfix">
						<span class="zp-10">Preview</span>
						<span class="zp-20">Title</span>
						<span class="zp-15">Type</span>
						<span class="zp-25">Filename</span>
						<span class="zp-10">Dimensions</span>
						<span class="zp-10">Uploaded</span>
						<span class="zp-10">Actions</span>
					</li>

				{% for id, rank in result %}
					{% with m.rsc[id] as r %}
						{% with r.medium as medium %}
						<li id="{{ #li.id }}" {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
							<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
								<span class="zp-10">{% image medium width=80 height=60 crop %}&nbsp;</span>
								<span class="zp-20">{{ r.title|striptags|default:"<em>untitled</em>" }}</span>
								<span class="zp-15">{{ medium.mime|default:"&nbsp;" }}</span>
								<span class="zp-25">{{ medium.filename|default:"-" }}</span>
								<span class="zp-10">{{ medium.width }} x {{ medium.height }}</span>
								<span class="zp-10">{{ medium.created|date:"M d, H:i"|default:"&nbsp;" }}</span>
								<span class="zp-10">
									{% button text="delete" disabled=r.is_protected action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
									{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
								</span>
							</a>
						</li>
						{% endwith %}
					{% endwith %}
				{% empty %}
					<li>
						No media found.
					</li>
				{% endfor %}

				</ul>

				{% pager result=result dispatch="admin_media" qargs %}

			{% endwith %}
		</div>
	</div>
{% endblock %}