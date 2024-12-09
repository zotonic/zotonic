{# Called by the action_unlink after removing an edge. Enables an undo of the action #}
<div class="alert alert-info clearfix edit-message" id="{{ #notice }}">
	{% button text=_"undo" class="btn btn-primary btn-xs pull-right"
			action={link
				subject_id=subject_id
				predicate=predicate
				object_id=object_id
				action=action
				edge_template=edge_template
			}
			action={hide}
			action={fade_out target=#notice}
		%}

	{_ The page _} “{{ object_id.title|default:object_id.short_title }}” {_ has been disconnected._}
</div>
