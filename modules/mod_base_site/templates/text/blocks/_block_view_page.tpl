{% with blk.rsc_id as id %}
{% if id.exists and id.is_visible %}
	{% catinclude "blocks/_block_view_page_"++blk.style++".tpl" id blk=blk %}
{% endif %}
{% endwith %}