{% extends "admin_base.tpl" %}

{% block title %}{_ Media _}{% endblock %}

{% block search %}
<div class="right">
    {% all include "_admin_headeritem.tpl" %}
    <form class="headeritem" action="{% url admin_media %}" method="get">
        <input type="hidden" name="qsort" value="{{ q.qsort }}" />
        <input type="hidden" name="qcat" value="{{ q.qcat }}" />
        <div class="search-wrapper">
            <input type="text" name="qs" value="{{q.qs|escape}}" />
        </div>
    </form>
</div>
{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Media _}</h2>

			<div class="clearfix">
				{% button
						text=_"Make a new media item" 
						action={dialog_media_upload}
				%}
			</div>

			<hr />
			
			<p>{_ Media encompasses all uploaded images, movies and documents. Media can be attached to pages. _}</p>
			
			{% with m.search.paged[{query cat="media" text=q.qs page=q.page sort=q.qsort|default:"-created"}] as result %}

				{% pager result=result dispatch="admin_media" qargs %}
				<h3 class="above-list">{_ Media overview _}</h3>

				<ul class="media-list short-list">
					<li class="headers clearfix">
						<span class="zp-10">{_ Preview _}</span>
 						<span class="zp-20">{% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}</span>
						<span class="zp-15">{_ Type _}</span>
						<span class="zp-25">{_ Filename _}</span>
						<span class="zp-10">{_ Dimensions _}</span>
						<span class="zp-10">{% include "_admin_sort_header.tpl" field="created" caption=_"Uploaded" %}</span>
						<span class="zp-10">{_ Actions _}</span>
					</li>

				{% for id in result %}
					{% if m.rsc[id].is_visible %}
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
									{% button text=_"delete" disabled=r.is_protected action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
									{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
								</span>
							</a>
						</li>
						{% endwith %}
					{% endwith %}
					{% endif %}
				{% empty %}
					<li>
						{_ No media found. _}
					</li>
				{% endfor %}

				</ul>

				{% pager result=result dispatch="admin_media" qargs %}

			{% endwith %}
		</div>
	</div>
{% endblock %}
