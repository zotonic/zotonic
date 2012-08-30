{% with blk.rsc_id as id %}
{% if id.exists and id.is_visible %}
	{% if blk.style == 'quote' %}
		{% catinclude "blocks/_block_view_page_quote.tpl" id blk=blk %}
	{% elseif blk.style == 'info' %}
		{% catinclude "blocks/_block_view_page_info.tpl" id blk=blk %}
	{% elseif blk.style == 'aside' %}
		{% catinclude "blocks/_block_view_page_aside.tpl" id blk=blk %}
	{% else %}
		{% catinclude "blocks/_block_view_page_inline.tpl" id blk=blk %}
	{% endif %}
{% endif %}
{% endwith %}