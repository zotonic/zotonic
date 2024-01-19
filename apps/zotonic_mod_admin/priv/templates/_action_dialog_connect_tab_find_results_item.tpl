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
        <div class="thumbnail{% if depict %} z-image-thumbnail{% endif %}{% if predicate %} thumbnail-{{ predicate }}{% endif %}{% if is_connected %} thumbnail-connected{% endif %}{% if is_linkable %} thumbnail-linkable{% endif %}{% if not id.is_published %} unpublished{% endif %}" data-id="{{ id }}">
        {% if depict %}
            <div class="z-thumbnail-image">
                {%
                    image
                    depict
                    mediaclass="admin-list-overview"
                    class="thumb pull-left"
                %}
            </div>
        {% endif %}

        <div class="z-thumbnail-text">
            {% block item_text %}
                <h6>{{ id.category_id.title }}</h6>

                <h5>{{ id.title|default:id.short_title|default:_"Untitled"|truncate:45 }}</h5>

                {% if id.summary %}
                    <p>{{ id|summary:80|striptags }}</p>
                {% endif %}

                {% if id.medium.filename|split:"/"|last as filename %}
                    <div class="z-thumbnail-filename" title="{{ filename }}">
                        <span class="glyphicon glyphicon-file"></span> {{ filename }}
                    </div>
                {% endif %}
            {% endblock %}
        </div>
    </div>
    {% endwith %}
    {% endwith %}
    {% endwith %}
</div>
