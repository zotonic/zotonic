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
    <div class="item{% if depict %} z-image-item{% endif %}{% if predicate %} item-{{ predicate }}{% endif %}{% if is_connected %} item-connected{% endif %}{% if is_linkable %} item-linkable{% endif %}{% if not id.is_published %} unpublished{% endif %}" data-id="{{ id }}">
        {%
            image
            depict
            mediaclass="admin-list-overview"
            class="thumb pull-left"
        %}
        <div class="z-item-text">
            {% block item_text %}
                <h6>{{ id.category_id.title }}</h6>
                <h5>{{ id.title }}</h5>
                <p>{{ id|summary:80 }}</p>
            {% endblock %}
            {% block item_actions %}
            <p class="rsc-actions">
{% comment %}
                <a href="#" class="btn btn-default action-preview">{_ Preview _}</a>
                {% if id.is_editable %}
                    <a href="#" class="btn btn-default action-edit">{_ Edit _}</a>
                {% endif %}
{% endcomment %}
                {% if is_zlink %}
                    <a href="#" class="btn btn-primary action-connect">{_ Link _}</a>
                {% elseif predicate and is_linkable %}
                    <a href="#" class="btn btn-primary action-connect">{_ Connect _}</a>
                {% endif %}
            </p>
            {% endblock %}
        </div>
    </div>
{% endwith %}
{% endwith %}
{% endwith %}
