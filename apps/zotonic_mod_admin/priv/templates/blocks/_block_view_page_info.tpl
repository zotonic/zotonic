<p class="block-info">
	<a href="{{ id.page_url }}" id="{{ #info }}"><i class="glyphicon glyphicon-info-sign"></i> {{ id.title }}</a>
    {% wire id=#info
            action={overlay_open
                template="page-parts/_dialog_qa.tpl"
                id=id
            }
    %}
</p>
