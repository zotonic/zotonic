{% if media.original == `missing` or media.preview == `missing` %}
    <div class="alert alert-warning" role="status">
        <strong>{_ Some media files cannot be recovered. _}</strong>
        {% if media.original == `missing` %}
            <p>{_ The original file for this version is no longer available. The page can be restored, but its media may be empty. _}</p>
        {% endif %}
        {% if media.preview == `missing` %}
            <p>{_ The saved preview image for this version is no longer available. _}</p>
        {% endif %}
        {% if page_exists %}
            <p>{_ Restoring this version can replace the media currently attached to the page. _}</p>
        {% endif %}
    </div>
{% endif %}
{% if media.original == `unknown` or media.preview == `unknown` %}
    <p class="alert alert-warning" role="status">
        {_ Media file availability could not be confirmed. You can continue restoring the page, but its media or saved preview image might not be recovered. _}
    </p>
{% endif %}
{% if media.original == `remote` or media.preview == `remote` %}
    <p class="help-block">
        {_ Media files are registered in the remote file store. Their availability has not been checked with the storage server. _}
    </p>
{% endif %}
