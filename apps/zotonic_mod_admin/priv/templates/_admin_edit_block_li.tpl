<li id="{{ #block }}" class="block do_adminwidget" data-minifier="1">
    <div class="widget">
        <div class="widget-header" title="{_ Drag to change position _}">
            &nbsp;
            <input type="text" class="block-name"
                   name="block-{{#s}}-name" id="block-{{#s}}-name"
                   value="{{ blk.name|escape }}"
                   title="{_ Block name _}"
                   placeholder="{_ name _}"
                   noautocomplete
            />
            <input type="hidden" class="block-type" name="block-{{#s}}-type" value="{{ blk.type }}" />
            &nbsp;
            {{ blk.type|make_list|capfirst|replace:"_":" " }} {_ block _}
            <a title="{_ Disconnect _}" class="z-btn-remove block-remove"></a>
        </div>
        <div class="widget-contentx">
            {% optional include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=#s blk=blk id=id is_new=is_new %}
        </div>
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
{% endjavascript %}
{% endif %}
