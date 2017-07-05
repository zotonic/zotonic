<thead>
    <tr>
        <th>{_ Status _}</th>
        <th>{_ URL _}</th>
        <th>{_ Actions _}</th>
    </tr>
</thead>
<tbody>
    {% for name, status, childspec, pid, time in sites %}
        {% if name /= 'zotonic_site_status' and (has_user or not configs[name].hidden) %}
        <tr id="site-{{ name }}" class="{{ status }}">
            <td>
                <span class="btn-xs waiting status">{_ Waiting _}</span>
                <span class="btn-xs running status">{_ Running _}</span>
                <span class="btn-xs retrying status">{_ Retrying _}</span>
                <span class="btn-xs failed status">{_ Failed _}</span>
                <span class="btn-xs stopped status">{_ Stopped _}</span>
            </td>

            <td>
                <a href="{{ configs[name].absurl|escape }}">{{ configs[name].absurl|escape }}</a>
                <small>({{ name }})</small>
            </td>

            {% if has_user %}
            <td>
                {% button
                    text=_"start"
                    class="start btn btn-default btn-xs"
                    title=_"Start the site."
                    postback={site_start site=name}
                %}

                {% button
                    text=_"stop"
                    class="stop btn btn-default btn-xs"
                    title=_"Stop the site."
                    postback={site_stop site=name}
                %}

                {% button
                    text=_"restart"
                    class="restart btn btn-default btn-xs"
                    title=_"Restart the site, all users will be logged off."
                    postback={site_restart site=name}
                %}

                {% button
                    text=_"flush"
                    class="flush btn btn-default btn-xs"
                    title=_"Flush and reload all settings, templates etc."
                    postback={site_flush site=name}
                %}

                {% button
                    text=_"admin"
                    class="admin btn btn-default btn-xs"
                    title=_"Visit the admin page for this site."
                    postback={site_admin site=name}
                %}

                {% all include "_z_status_button.tpl" %}
            </td>
        {% endif %}
        </tr>
        {% endif %}
    {% endfor %}
</tbody>
