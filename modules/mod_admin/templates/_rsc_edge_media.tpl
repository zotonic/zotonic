{# Show a thumbnail with an unlink option. Used in the admin_edit #}

			{% sortable id=#unlink_wrapper tag=edge_id %}
			<li id="{{ #unlink_wrapper }}" class="edit_media clearfix">
				{% image object_id width=187 height=200 extent %}

				{% with m.rsc[object_id].title|striptags|default:_"untitled" as title %}
				<div class="media-unlink-wrapper clearfix">
					<div class="rsc-edge do_unlink">
						<span class="clearfix">
							<span class="unlink-mover"></span>
							<span id="{{ #unlink.object_id }}" class="unlink-cross"></span>
							<span class="unlink-item"><a href="{% url admin_edit_rsc id=object_id %}" id="{{ #edit }}" title="{{ title }}">{{ title }}</a></span>
						</span>
					</div>
				</div>
				{% endwith %}

			</li>

{% wire id=#unlink.object_id
action={unlink 
    subject_id=subject_id 
    predicate="depiction" 
    object_id=object_id 
    hide=#unlink_wrapper
    undo_message_id=unlink_message 
    undo_action={postback postback={reload_media rsc_id=id div_id=div_id} delegate="resource_admin_edit"}} 
%}
{% wire id=#edit target=#unlink_wrapper action={dialog_edit_basics edge_id=edge_id template="_rsc_edge_media.tpl"} %}
