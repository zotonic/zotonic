{% if id.is_editable %}
    {% button
        text=_"Edit image"
        class="btn btn-default"
        element="a"
        action={overlay_open
            template="_overlay_image_edit.tpl"
            id=id
            class="dark image-edit-overlay"
        }
    %}
{% endif %}

