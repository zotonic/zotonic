{% with id.depiction as depict %}
{% with is_connected
        or (predicate
            and (   (subject_id and m.edge.id[subject_id][predicate][id])
                 or (object_id and  m.edge.id[id][predicate][object_id])))
   as is_connected
%}
{% with (subject_id and m.acl.is_allowed.link[subject_id])
     or (object_id and m.acl.is_allowed.link[id])
   as is_linkable
%}

    <div class="item card mb-3{% if depict %} z-image-thumbnail{% endif %}{% if predicate %} thumbnail-{{ predicate }}{% endif %}{% if is_connected %} thumbnail-connected{% endif %}{% if is_linkable %} thumbnail-linkable{% endif %}{% if not id.is_published %} unpublished{% endif %}" data-id="{{ id }}">
        <div class="row g-0">
            {% if depict %}
                <div class="col-md-4">
                    {%
                        image
                        depict
                        mediaclass="admin-list-overview"
                        class="img-fluid rounded-start"
                    %}
                </div>
            {% endif %}

            <div class="col-md-8">
                <div class="card-body">
                    {% block item_text %}
                        <h6>{{ id.category_id.title }}</h6>

                        <h5>{{ id.title|default:id.short_title|default:_"Untitled" }}</h5>

                        {% if id.summary %}
                            <p>{{ id|summary:80|striptags }}</p>
                        {% endif %}

                        {% if id.medium.filename|split:"/"|last as filename %}
                            <div class="z-thumbnail-filename" title="{{ filename }}">
                                <span class="glyphicon glyphicon-file"></span> {{ filename }}
                            </div>
                        {% endif %}
                    {% endblock %}
                    
                    {% block item_actions %}
                        <p class="rsc-actions">
                            {% if intent == "select" %}
                                <a href="#" class="btn btn-primary action-connect">{_ Select _}</a>
                            {% elseif intent == "connect" and is_linkable %}
                                <a href="#" class="btn btn-primary btn-sm action-connect">{_ Connect _}</a>
                            {% endif %}
                        </p>
                    {% endblock %}
                </div>
            </div>
        </div>
    </div>
{% endwith %}
{% endwith %}
{% endwith %}
