<input type="hidden" class="block-type" name="block-{{#s}}-type" value="{{ blk.type|default:block_type }}" />
{% include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=#s blk=blk id=id is_editable=is_editable is_new=is_new %}

{% if is_new %}
{% javascript %}
    $("#{{ element_id }}")
        .effect('highlight')
        .find(".block-name")
        .attr('name', 'block-{{#s}}-name')
        .attr('id', 'block-{{#s}}-name');

    setTimeout(function() { 
        z_editor_init();
        z_admin_ensure_block_names();
    }, 100);
{% endjavascript %}
{% endif %}
