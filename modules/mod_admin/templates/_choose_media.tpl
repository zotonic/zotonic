{# Used in the TinyMCE editor to insert a media item in the running text. Expects the 'ids' argument to be set to a number of media items. #}

<ul id="{{ #media }}" class="thumbnails">
    {% for media_id in ids %}
    <li class="span2">
        <div class="thumbnail">
            <a id="{{ #choose.media_id }}" href="#">
	        {% with m.rsc[media_id].medium as medium %}
	        {% with m.rsc[media_id].title|striptags|default:_"untitled" as title %}
	        {% image media_id width=130 height=130 crop title=title %}
	        {% endwith %}
	        {% wire id=#choose.media_id action={zmedia_choose id=media_id} %}
	        {% endwith %}
            </a>
        </div>
    </li>
    {% endfor %}
</ul>
