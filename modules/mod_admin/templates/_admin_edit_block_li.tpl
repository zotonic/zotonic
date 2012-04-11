{% with is_editable|default:id.is_editable as is_editable %}
<li id="{{ #block }}" class="block do_adminwidget">
    <h3 class="widget-header">
        <i title="{_ Drag to change position _}" class="icon-move"></i>
        <span title="{_ Disconnect _}" class="icon-remove"></span>
        <span>{{ blk.type|make_list|capfirst }} {_ block _}</span>
        <input type="text" class="block-name" name="block-{{#s}}-name" id="block-{{#s}}-name" value="{{ blk.name|escape }}" title="{_ Block name _}" />
    </h3>
    <div class="widget-content">
        <input type="hidden" name="block-{{#s}}-type" value="{{ blk.type }}" />
        {% if blk.type == 'text' or not blk %}
            {% include "_admin_edit_block_li_text.tpl" name=#s %}
        {% elseif blk.type == 'header' %}
            {% include "_admin_edit_block_li_header.tpl" name=#s %}
        {% endif %}
    </div>
    {% if is_new %}
        {% wire action={script script="z_tinymce_init();" } %}
    {% endif %}
    
    {% include "_admin_edit_block_addblock.tpl" %}
</li>
{% endwith %}
