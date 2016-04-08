<input id="{{ #collab_group_id }}" type="hidden" name="collab_group_id" value="{{ id }}" />

<p>
    <span class="z-icon z-icon-user"></span> {{ id.title }}
    <a href="#" id="{{ #collab_group_id_rm }}" class="btn">&times;</a>
</p>

{% javascript %}
    $('#{{ #collab_group_id }}')
        .closest('form')
        .find('select[name=content_group_id]')
        .closest('div')
        .hide();

    $('#{{ #collab_group_id_rm }}').click(function() {
        $('#{{ #collab_group_id }}')
            .closest('form')
            .find('select[name=content_group_id]')
            .closest('div')
            .show();
        $('#{{ #collab_group_id }}')
            .closest('div')
            .html('');
    });
{% endjavascript %}
