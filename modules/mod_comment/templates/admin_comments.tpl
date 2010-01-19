{% extends "admin_base.tpl" %}

{% block title %}
Recent Comments
{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Comments</h2>

			<h3 class="above-list">Recent comments</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-15">Added on</span>
					<span class="zp-15">Page</span>
					<span class="zp-25">Message</span>
					<span class="zp-15">Name</span>
					<span class="zp-15">Email</span>
					<span class="zp-15">Options</span>
				</li>
			{% with m.search.paged[{recent_comments}] as result %}
				{% for comment in result %}
					{% with comment.id as id %}
					<li {% if not comment.is_visible %}class="unpublished" {% endif %}>
						<a id="{{ #comment.id }}" href="{{ m.rsc[comment.rsc_id].page_url }}#comment-{{ id }}" class="clearfix">
							<span class="zp-15">{{ comment.created|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[comment.rsc_id].title|truncate:20 }}</span>
							<span class="zp-25">{{ comment.message|striptags|truncate:40 }}</span>
							<span class="zp-15">{{ comment.name|truncate:20 }}</span>
							<span class="zp-15">{{ comment.email|truncate:20|escape }}</span>
							<span class="zp-15">
	                            {% button text="view" action={redirect location=[m.rsc[comment.rsc_id].page_url,"#comment-",id|format_integer]|join } %}
								{% button text="delete" postback={comment_delete id=id on_success={slide_fade_out id=#comment.id}} %}
							</span>
						</a>
					</li>
					{% endwith %}
				{% empty %}
					<li>There are no comments.</li>
				{% endfor %}
				</ul>
			
				{% pager result=result dispatch="admin_comments" qargs %}

			{% endwith %}
			
		</div>
	</div>
{% endblock %}