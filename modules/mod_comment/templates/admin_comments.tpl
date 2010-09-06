{% extends "admin_base.tpl" %}

{% block title %}
{_ Recent Comments _}
{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Comments _}</h2>

			<h3 class="above-list">{_ Recent comments _}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-15">{_ Added on _}</span>
					<span class="zp-15">{_ Page _}</span>
					<span class="zp-25">{_ Message _}</span>
					<span class="zp-15">{_ Name _}</span>
					<span class="zp-15">{_ Email _}</span>
					<span class="zp-15">{_ Options _}</span>
				</li>
			{% with m.search.paged[{recent_comments page=q.page}] as result %}
				{% for comment in result %}
					{% with comment.id as id %}
					<li {% if not comment.is_visible %}class="unpublished" {% endif %}>
						<a id="{{ #comment.id }}" href="{{ m.rsc[comment.rsc_id].page_url }}#comment-{{ id }}" class="clearfix">
							<span class="zp-15">{{ comment.created|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[comment.rsc_id].title|truncate:20 }}</span>
							<span class="zp-25">{{ comment.message|striptags|truncate:40 }}</span>
							{% if comment.user_id %}
								<span class="zp-30">{{ m.rsc[comment.user_id].title }} [#{{ comment.user_id }}]</span>
							{% else %}
								<span class="zp-15">{{ comment.name|truncate:20 }}</span>
								<span class="zp-15" title="{{ comment.email|truncate }}">{{ comment.email|truncate:20|escape }}</span>
							{% endif %}
							<span class="zp-15">
	                            {% button text=_"view" action={redirect location=[m.rsc[comment.rsc_id].page_url,"#comment-",id|format_integer]|join } %}
								{% button text=_"delete" postback={comment_delete id=id on_success={slide_fade_out target=#comment.id}} %}
							</span>
						</a>
					</li>
					{% endwith %}
				{% empty %}
					<li>{_ There are no comments. _}</li>
				{% endfor %}
				</ul>
			
				{% pager result=result dispatch="admin_comments" qargs %}

			{% endwith %}
			
		</div>
	</div>
{% endblock %}