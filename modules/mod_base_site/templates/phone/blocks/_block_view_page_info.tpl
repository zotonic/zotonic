<p class="block-info">
	<a href="{{ id.page_url }}" id="{{ #info }}"><i class=" icon-info-sign"></i> {{ id.title }}</a>
	{% wire id=#info action={dialog_open title=id.title id=id template="blocks/_dialog_block_view_page_info.tpl"} %}
</p>
