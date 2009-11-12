{# Used on the resource edit page and by the medium upload event.  Show all connected media. #}

	<div id="{{ #unlink_message }}"></div>
	
	{% sorter id=#media tag={object_sorter predicate="depiction" id=id} placeholder="ui-sortable-placeholder" %}
	<ul id="{{ #media }}" class="media-sorter">
		{% for media_id, edge_id in m.edge.o[id].depiction %}
			{% sortable id=#medium.media_id tag=edge_id %}
			<li id="{{ #medium.media_id }}" class="edit_media clearfix">
				{% with m.rsc[media_id].medium as medium %}
					{% image medium width=187 height=200 crop %}

					{% with m.rsc[media_id].title|striptags|default:"untitled" as title %}
					<div class="media-unlink-wrapper">
						<div class="rsc-edge do_unlink">
							<span class="clearfix">
								<span class="unlink-mover"></span>
								<span id="{{ #unlink.media_id }}" class="unlink-cross do_tooltip" title="Disconnect {{title}}."></span>
								<span class="unlink-item"><a href="{% url admin_edit_rsc id=media_id %}">{{ title }}</a></span>
							</span>
						</div>
					</div>
					{% endwith %}

					{% wire id=#unlink.media_id
							action={unlink 
										subject_id=id 
										predicate="depiction" 
										object_id=media_id 
										hide=#medium.media_id
										undo_message_id=#unlink_message 
										undo_action={postback postback={reload_media rsc_id=id div_id=div_id} delegate="resource_admin_edit"}} 
					%}

				{% endwith %}
			</li>
		{% endfor %}
	</ul>
