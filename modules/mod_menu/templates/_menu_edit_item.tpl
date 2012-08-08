{% if id %}<li id="{{ #menu.c }}-{{ id }}" class="menu-item">
	<div id="{{ menu_id|default:#menu.id }}" data-page-id="{{ id }}">
	    <img class="grippy" src="/lib/images/grippy.png" title="{_ Drag me _}" />
	    <span class="title-{{id}}">{{ id.title }}</span>

	    <span class="pull-right btn-group">
	        <a href="#" class="btn dropdown-toggle" data-toggle="dropdown"><i class="icon-cog"></i> <span class="caret"></span></a>
			<ul class="dropdown-menu">
			    <li><a href="#" data-where="before">&uarr; {_ Add before _}</a></li>
			    <li><a href="#" data-where="below">&rarr; {_ Add below _}</a></li>
			    <li><a href="#" data-where="after">&darr; {_ Add after _}</a></li>
			    <li class="divider"></li>
			    <li><a href="#" data-where="remove">{_ Remove _}</a></li>
			</ul>
	    </span>

	    <span class="pull-right btn-group">
	        <a href="#" class="btn menu-edit">{_ Edit _}</a>
	    </span>
	</div>

	{% if action == `down` %}
		<ul class="menu-submenu">
	{% else %}
		</li>
	{% endif %}
{% else %}
</ul></li>{% endif %}