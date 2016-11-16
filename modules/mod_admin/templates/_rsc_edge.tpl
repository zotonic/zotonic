{#
Show an object with an unlink option. Used in the admin_edit

Params:
- object_id (integer) resource id to disconnect
- subject_id (integer) resource where id is connected to
- edge_id (integer) connection resource
- unlink_action (optional) action to be called after disconnecting
- undo_message_id (optional) id of element for unlink/undo message
#}
{% with m.rsc[object_id].title as title %}
{% sortable id=#unlink_wrapper tag=edge_id %}
<li id="{{ #unlink_wrapper }}" class="menu-item">
    <div>
	    <i class="z-icon z-icon-drag"></i>
        <a id="{{ #edit }}" href="{% url admin_edit_rsc id=object_id %}" title="{_ Edit _}">
            {% catinclude "_rsc_edge_item.tpl" object_id %}
       	</a>
        <button type="button" id="{{ #unlink }}" title="{_ Disconnect _}" class="z-btn-remove"></button>
    </div>
</li>
{% endwith %}

{% wire id=#unlink
    action={unlink
        subject_id=subject_id
        edge_id=edge_id
        hide=#unlink_wrapper
        undo_message_id=undo_message_id
        action=unlink_action
    }
%}
{% wire id=#edit
    target=#unlink_wrapper
    action={dialog_edit_basics
        edge_id=edge_id
    }
%}
