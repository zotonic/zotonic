{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="admin-form block-page">
    <a class="btn page-connect pull-right" href="#connect">{_ Connect a page _}</a>
    <div class="rsc-item-wrapper" id="{{ #wrap }}">
		{% include "_rsc_item.tpl" id=blk.rsc_id %}
	</div>
	<input type="hidden" id="block-{{name}}-rsc_id" name="block-{{name}}-rsc_id" value="{{ blk.rsc_id }}" />
</fieldset>
{% endwith %}

{% include "_admin_edit_block_show_as.tpl" %}

{% endblock %}
