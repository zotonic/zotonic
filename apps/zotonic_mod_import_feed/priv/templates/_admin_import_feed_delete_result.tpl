{% if feed_deleted %}
    <p>{_ The feed and its import history have been deleted. _}</p>
{% else %}
    <p>{_ The feed is paused. _}</p>
{% endif %}
{% if not feed_deleted or delete_content %}
    <ul>
        <li>{_ Deleted: _} {{ counts.deleted }}</li>
        <li>{_ Kept: _} {{ counts.skipped }}</li>
        <li>{_ Failed: _} {{ counts.failed }}</li>
    </ul>
    <p>{_ Protected resources and resources you cannot delete are kept. A resource is also kept if deleting it could cause automatic deletion of dependent content you cannot delete. _}</p>
{% else %}
    <p>{_ All imported content has been kept. _}</p>
{% endif %}
<div class="modal-footer">
    {% if feed_deleted %}
        {% button class="btn btn-primary" text=_"Done" action={redirect dispatch=`admin_import_feed`} %}
    {% else %}
        {% button class="btn btn-primary" text=_"Done" action={reload} %}
    {% endif %}
</div>
