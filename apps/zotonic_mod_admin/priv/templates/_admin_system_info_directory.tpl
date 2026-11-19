<code>{{ directory|escape }}</code>
{% if can_open_directory and directory %}
    {% button class="btn btn-default btn-xs" icon="glyphicon glyphicon-folder-open" text=_"Open" title=_"View this directory in the file manager on the Zotonic server's desktop"
        postback={open_directory directory=directory_key} delegate=`mod_development` %}
{% endif %}
