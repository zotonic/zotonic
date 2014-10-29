<div class="col-lg-4 col-md-4">
    {% with id.medium as medium %}
        <div class="thumbnail{% if medium %} z-image-thumbnail{% endif %}" data-id="{{ id }}">
            {%
                image
                medium
                mediaclass="admin-list-overview"
                class="thumb pull-left"
            %}
            <h6>{{ id.category_id.title }}</h6>
            <h5>{{ id.title }}</h5>
            <p>{{ id|summary:50 }}</p>
        </div>
    {% endwith %}
</div>
