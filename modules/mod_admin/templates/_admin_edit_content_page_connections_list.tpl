{#
Params:
- id: (integer) page id
- predicate: (atom) predicate name
- button_label (optional) (string)
- button_class (optional) (string); default: ""
- dialog_title_add (optional) (string); default: "Add a connection: Predicate Name"
- tabs_enabled (optional) (list of strings), f.i. ["new", "find"]; default: undefined
- callback (optional) (string) JavaScript function to be called after connecting
- action (optional) action to be called after succesful connecting
- unlink_action (optional) action to be called after succesful disconnecting
- undo_message_id (opional) id of the div for the unlink message, defaults to "unlink-undo-message"
- list_id (optional) (string) connection list identifier

Config key:
- mod_admin.edge_list_max_length -- max number of items shown per predicate
#}
{% with m.config.mod_admin.edge_list_max_length.value|default:100|to_integer as edge_list_max_length %}
{% with list_id|default:#list_id as list_id %}
<div class="unlink-wrapper">
    {% with m.edge.o[id][predicate] as edges %}
      {% with edges|length > edge_list_max_length as is_list_truncated %}
        {% if not is_list_truncated %}
          {% sorter id=list_id
                    tag={object_sorter predicate=predicate id=id}
                    group="edges"
                    delegate=delegate|default:`controller_admin_edit`
          %}
        {% endif %}
        <ul id="{{ list_id }}" class="tree-list connections-list">
          {% include "_rsc_edge_list.tpl" id=id predicate=predicate
                unlink_action=unlink_action undo_message_id=undo_message_id
                is_list_truncated=is_list_truncated
                edge_list_max_length=edge_list_max_length
          %}
        </ul>
      {% endwith %}
    {% endwith %}
</div>
{% endwith %}
{% endwith %}

{% if m.acl.is_allowed.link[id] %}
  <a id="{{ #connect.predicate }}" href="#connect" class="{{ button_class|default:"" }}">{{ button_label|default:_"+ add" }}</a>
    {% wire
       id=#connect.predicate
       action={
          dialog_open
          template="_action_dialog_connect.tpl"
          title=dialog_title_add|default:[_"Add a connection: ", predicate.title]
          subject_id=id
          predicate=predicate
          tabs_enabled=tabs_enabled
          callback=callback
          action=action
          unlink_action=unlink_action
          undo_message_id=undo_message_id|default:"unlink-undo-message"
          center=0
          width="large"
        }
    %}
{% endif %}
