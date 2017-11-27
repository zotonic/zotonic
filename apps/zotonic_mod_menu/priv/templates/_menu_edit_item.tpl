{% if id %}
<li id="{{ #menu.c }}-{{ id }}" class="menu-item {% if action == `down` %}has-submenu{% endif %} submenu-open">
	<div id="{{ menu_id|default:#menu.id }}" data-page-id="{{ id }}">
		<a class="menu-toggle" href="#toggle"><i class="menu-expand z-icon z-icon-plus"></i><i class="menu-collapse z-icon z-icon-minus"></i></a>
	    <span class="title-{{id}}">
	    	{% image id mediaclass="admin-list-dashboard" %}
	    	<span class="menu-label">{{ id.short_title|default:id.title }}</span>
	    	<span class="category">{{ id.category_id.title }}</span>
	    </span>

		<i class="warning glyphicon glyphicon-eye-close" {% if id.is_published %}style="display: none"{% endif %}></i>

        <span class="btns">
            <span class="btn-group">
                <a href="#" class="btn btn-default btn-xs menu-edit">{_ Edit _}</a>

                {% if editable %}
                    <a href="#" class="btn btn-default btn-xs dropdown-toggle" data-toggle="dropdown"><i class="glyphicon glyphicon-cog"></i> <span class="caret"></span></a>
                    <ul class="dropdown-menu dropdown-menu-right">
                        <li><a href="#" data-where="before">&uarr; {_ Add before _}</a></li>
                        <li><a href="#" data-where="below">&rarr; {_ Add below _}</a></li>
                        <li><a href="#" data-where="after">&darr; {_ Add after _}</a></li>
                        <li class="divider"></li>
                        {% if m.acl.is_allowed.insert[id.category.name] %}
                            <li><a href="#" data-where="copy">{_ Copy _}</a></li>
                        {% endif %}
                        <li><a href="#" data-where="remove">{_ Remove _}</a></li>
                    </ul>
                {% endif %}
            </span>
        </span>
	</div>

	{% if action == `down` %}
		<ul>
	{% else %}
		</li>
	{% endif %}
{% else %}
	</ul>
	</li>
{% endif %}
