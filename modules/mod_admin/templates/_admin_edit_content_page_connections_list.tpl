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
- list_id (optional) (string) connection list identifier
#}
{% with list_id|default:("links-" ++ id ++ "-" ++ predicate) as list_id %}
<div class="unlink-wrapper">
    {% sorter id=["links",id|format_integer,predicate]|join:"-" 
              tag={object_sorter predicate=predicate id=id} 
              group="edges"
              delegate=delegate|default:`controller_admin_edit`
    %}
    <ul id="{{ list_id }}" class="tree-list connections-list">
      {% include "_rsc_edge_list.tpl" id=id predicate=predicate unlink_action=unlink_action %}
    </ul>
</div>

{% wire
    name=list_id
    action={update
        target=list_id
        template="_rsc_edge_list.tpl"
        id=id
        predicate=predicate
        unlink_action=unlink_action
    }
%}
{% endwith %}

{% if is_editable %}
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
          center=0
        }
    %}
{% endif %}
