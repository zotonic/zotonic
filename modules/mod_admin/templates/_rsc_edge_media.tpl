{# Show a thumbnail with an unlink option. Used in the admin_edit #}

{% sortable id=#unlink_wrapper tag=edge_id %}
<li id="{{ #unlink_wrapper }}">
    <div class="thumbnail">
        {% image object_id mediaclass="admin-rsc-edge-media" %}

        <div class="pull-right">
            <button id="{{ #unlink.object_id }}" class="btn btn-mini" title="{_ Disconnect _} {{title}}."><i class="icon-remove"></i></button>
        </div>
        <div class="caption">
            {% with m.rsc[object_id].title|striptags|default:_"untitled" as title %}
            <a href="#" id="{{ #edit }}">{{ title }}</a>
            {% endwith %}
	</div>
    </div>
</li>

{% wire id=#unlink.object_id
        action={unlink 
            subject_id=subject_id 
            predicate="depiction" 
            object_id=object_id 
            hide=#unlink_wrapper
            undo_message_id=unlink_message 
            undo_action={postback postback={reload_media rsc_id=id div_id=["links-",id|make_list,"-depiction"]} 
                                  delegate="controller_admin_edit"}} 
%}
{% wire id=#edit target=#unlink_wrapper action={dialog_edit_basics edge_id=edge_id template="_rsc_edge_media.tpl"} %}
