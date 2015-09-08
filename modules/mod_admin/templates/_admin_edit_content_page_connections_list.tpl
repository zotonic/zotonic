{#
Params:
id
predicate: (predicate object) the predicate
predicate_name: (atom) predicate name
button_label (optional) (string)
dialog_title_add (optional) (string)
tabs_enabled (optional) (list of strings), f.i. ["new", "find"]
#}
<div class="unlink-wrapper">
    {% sorter id=["links",id|format_integer,predicate_name]|join:"-" 
              tag={object_sorter predicate=predicate_name id=id} 
              group="edges"
              delegate=`controller_admin_edit`
    %}
    <ul id="links-{{ id }}-{{ predicate_name }}" class="tree-list connections-list" data-reload-template="_rsc_edge_list.tpl">{% include "_rsc_edge_list.tpl" id=id predicate=predicate_name %}</ul>
</div>

{% if is_editable %}
  <a id="{{ #connect.predicate_name }}" href="#connect">{{ button_label|default:"+ " ++ _"add a connection" }}</a>
    {% wire
       id=#connect.predicate_name
       action={
          dialog_open
          template="_action_dialog_connect.tpl" 
          title=dialog_title_add|default:[_"Add a connection: ", predicate.title]
          subject_id=id
          predicate=predicate_name
          tabs_enabled=tabs_enabled
        }
    %}
{% endif %}
