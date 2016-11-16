{# Show a thumbnail with an unlink option. Used in the admin_edit #}

{% sortable id=#unlink_wrapper tag=edge_id %}
{% with (object_id.is_a.image and object_id.crop_center) as has_cropcenter %}
<li id="{{ #unlink_wrapper }}" class="{% if not object_id.is_published %}unpublished{% endif %}">
    {% if has_cropcenter %}
        {% with (1 / object_id.medium.width * object_id.medium.height) as ratio %}
        <figure class="do_cropcenter{hiddenInput:'#crop_center_{{ object_id }}', editable: false}" data-original-width="{{ object_id.medium.width }}" style="{% if ratio > 1 %}max-width: {{ 100 * 1/ratio }}%;{% else %}max-height: {{ 100 * ratio }}%;{% endif %}">
            <input type="hidden" name="crop_center" id="crop_center_{{ object_id }}" value="{{ object_id.crop_center }}" />
        {% endwith %}
    {% else %}
        <figure>
    {% endif %}
        {% image object_id mediaclass="admin-rsc-edge-media" %}
    </figure>
    <div class="bottom clearfix">
        <div class="caption">
            {% with m.rsc[object_id].title|striptags|default:_"untitled" as title %}
                <a href="#" id="{{ #edit }}">{{ object_id.cropcenter }} {{ title }}</a>
            {% endwith %}
	    </div>
	    <a id="{{ #unlink.object_id }}" class="z-btn-remove" title="{_ Disconnect _} {{ object_id.title }}"></a>
    </div>
</li>
{% endwith %}

{% wire id=#unlink.object_id
    action={
        unlink
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
                div_id=["links-",id|make_list,"-depiction"]
            }
            delegate="controller_admin_edit"
        }
    }
%}
{% wire
    id=#edit
    target=#unlink_wrapper
    action={
        dialog_edit_basics
        edge_id=edge_id
        template="_rsc_edge_media.tpl"
    }
%}
