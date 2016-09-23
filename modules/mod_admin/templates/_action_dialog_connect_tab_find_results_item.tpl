<div class="col-lg-4 col-md-4">
    {% with id.depiction as depict %}
    {% with is_connected
            or (predicate 
                and (   (subject_id and m.edge.id[subject_id][predicate][id])
                     or (object_id and  m.edge.id[id][predicate][object_id])))
       as is_connected
    %}
    {% with (subject_id and m.acl.is_allowed.link[subject_id])
         or (object_id and m.acl.is_allowed.link[object_id])
       as is_linkable
    %}
        <a href="{{ id.page_url }}" class="thumbnail{% if depict %} z-image-thumbnail{% endif %}{% if predicate %} thumbnail-{{ predicate }}{% endif %}{% if is_connected %} thumbnail-connected{% endif %}{% if is_linkable %} thumbnail-linkable{% endif %}{% if not id.is_published %} unpublished{% endif %}" data-id="{{ id }}">
            {%
                image
                depict
                mediaclass="admin-list-overview"
                class="thumb pull-left"
            %}
            <div class="z-thumbnail-text">
                <h6>{{ id.category_id.title }}</h6>
                <h5>{{ id.title }}</h5>
                <p>{{ id|summary:50 }}</p>
            </div>
        </a>
    {% endwith %}
    {% endwith %}
    {% endwith %}
</div>
