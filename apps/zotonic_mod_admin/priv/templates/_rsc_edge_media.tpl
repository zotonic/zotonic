{# Show a thumbnail with an unlink option. Used in the admin_edit #}

{% sortable id=#unlink_wrapper tag=edge_id %}
<li id="{{ #unlink_wrapper }}" class="{% if not object_id.is_published %}unpublished{% endif %}">
    <figure>
        {% image object_id mediaclass="admin-rsc-edge-media" alt=object_id.title title=object_id.title %}
    </figure>
    <a id="{{ #unlink.object_id }}" class="z-btn-remove" title="{_ Disconnect _} {{ object_id.title }}"></a>
    <div class="bottom clearfix">
        <div class="caption">
            {% with object_id.title|default:object_id.short_title|default:_"untitled" as title %}
                <a href="#" id="{{ #edit }}" title="{{ object_id.title }}">{{ title }}</a>
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
        undo_message_id=undo_message_id
        undo_action={
            postback
            postback={
                reload_media
                rsc_id=id
                div_id=["links-",id|to_binary,"-depiction"]
            }
            delegate="controller_admin_edit"
        }
    }
%}
{% wire
    id=#edit
    target=#unlink_wrapper
    action={dialog_edit_basics
        edge_id=edge_id
        template="_rsc_edge_media.tpl"
    }
%}
