{#
Show an object with an unlink option. Used in the admin_edit

Params:
- object_id (integer) resource id to disconnect
- subject_id (integer) resource where id is connected to
- edge_id (integer) connection resource
- unlink_action (optional) action to be called after disconnecting
- undo_message_id (optional) id of element for unlink/undo message
#}
{% if not is_list_truncated %}
    {% sortable id=#unlink_wrapper tag=edge_id %}
{% endif %}
<li id="{{ #unlink_wrapper }}" class="menu-item">
    <div class="menu-wrapper">
        {% if not is_list_truncated %}
           <div class="menu-drag"></div>
        {% endif %}
        <a id="{{ #edit }}" href="{% url admin_edit_rsc id=object_id %}" title="{_ Edit _}">
            {% catinclude "_rsc_edge_item.tpl" object_id %}
           </a>
        <button type="button" id="{{ #unlink }}" title="{_ Disconnect _}" class="z-btn-remove"></button>
    </div>
</li>
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
