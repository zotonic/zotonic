{# Used in the TinyMCE editor to insert a media item in the running text. Expects the 'ids' argument to be set to a number of media items. #}

<div class="choose-media-wrapper">
	<ul id="{{ #media }}" class="list clearfix">
		{% for media_id in ids %}
			<li id="{{ #medium.media_id }}" class="edit_media left clearfix">
				{% with m.rsc[media_id].medium as medium %}
					{% with m.rsc[media_id].title|striptags|default:_"untitled" as title %}
						<span id="{{ #choose.media_id }}" style="cursor: pointer">
							{% image media_id width=80 height=80 crop title=title %}
						</span>
					{% endwith %}
				
					{% wire id=#choose.media_id action={zmedia_choose id=media_id} %}
				{% endwith %}
			</li>
		{% endfor %}
	</ul>
</div>	
