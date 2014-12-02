<div class="col-lg-4 col-md-4">
    {% with id.depiction as depict %}
        <div class="thumbnail{% if medium %} z-image-thumbnail{% endif %}" data-id="{{ id }}">
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
        </div>
    {% endwith %}
</div>
