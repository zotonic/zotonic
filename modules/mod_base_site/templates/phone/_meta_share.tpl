{% if id %}
	<a id="{{ #share }}" href="#" title="{_ Share this page _}" class="btn btn-default btn-xs"><span class="z-icon z-icon-share"></span> {_ Share _}</a>
    {% wire id=#share action={dialog_open title=_"Share this page" template="_dialog_share_page.tpl" id=id} %}
{% endif %}
