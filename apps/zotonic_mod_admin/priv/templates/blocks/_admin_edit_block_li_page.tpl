{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}

{% block widget_content %}
<fieldset class="block-page">
    <a class="btn btn-outline-secondary page-connect pull-right" href="#connect">{_ Connect a page _}</a>
    <div class="rsc-item-wrapper" id="{{ #wrap }}">
        {% if blk.rsc_id %}
    		{% catinclude "_rsc_block_item.tpl" blk.rsc_id %}
        {% endif %}
	</div>
    <br><br>
    <a href="#disconnect" class="btn btn-sm btn-danger page-disconnect pull-right">{_ Remove _}</a>
    <br><br>
	<input type="hidden" id="block-{{name}}-rsc_id" name="blocks[].rsc_id" value="{{ blk.rsc_id }}" />
</fieldset>

{% include "_admin_edit_block_show_as.tpl" is_page_block %}

{% endblock %}


