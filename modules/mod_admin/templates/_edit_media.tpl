{# Used on the resource edit page and by the medium upload event.  Show all connected media. #}

<div id="{{ #unlink_message }}"></div>
	
{% sorter id=#media tag={object_sorter predicate="depiction" id=id} placeholder="ui-sortable-placeholder" %}
<ul id="{{ #media }}" class="media thumbnails">
    {% for object_id, edge_id in m.edge.o[id].depiction %}
    {% include "_rsc_edge_media.tpl" subject_id=id unlink_message=#unlink_message %}
    {% endfor %}
</ul>
