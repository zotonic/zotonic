<li id="{{ #block }}" class="block do_adminwidget" data-minifier="1">

    <!-- New block entry -->
    <input type="hidden" name="blocks[]." value="">
    <input type="hidden" class="block-type" name="blocks[].type" value="{{ blk.type }}">

    <!-- The widget with the name, includes the specific fields for the block type -->
    <div class="widget">
        <div class="widget-header label-floating" title="{_ Drag to change position _}">
            &nbsp;
            <input type="text"
                   class="block-name form-control form-control-small"
                   name="blocks[].name"
                   value="{{ blk.name|escape }}"
                   title="{_ Block name _}"
                   placeholder="{_ name _}"
                   autocomplete="off"
            >
            &nbsp;
            {{ blk.type|make_list|capfirst|replace:"_":" " }} {_ block _}
            <a title="{_ Disconnect _}" class="z-btn-remove block-remove"></a>
        </div>
        {% optional include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=#s blk=blk id=id is_new=is_new %}
    </div>
    {% if is_new %}
        {% javascript %}
            z_editor_init();
        {% endjavascript %}
    {% endif %}
    {% include "_admin_edit_block_addblock.tpl" %}
</li>

{% if is_new %}
{% javascript %}
    $("#{{ #block }} .widget").effect("highlight");
    z_admin_ensure_block_names();
    $("#{{ #block }}").closest('form').trigger('change');
{% endjavascript %}
{% endif %}
