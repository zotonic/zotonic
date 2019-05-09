<table id="sites" class="table sites-overview">
<thead>
    <tr>
        <th>{_ Status _}</th>
        <th>{_ URL _}</th>
        <th>{_ Actions _}</th>
    </tr>
</thead>
<tbody>
    {% for name, status in m.zotonic_status.sites_status %}
        {% if name /= 'zotonic_site_status' %}
            <tr id="site-{{ name }}" class="{{ status }}">
                <td>
                    <span class="btn-xs waiting status">{_ Waiting _}</span>
                    <span class="btn-xs running status">{_ Running _}</span>
                    <span class="btn-xs retrying status">{_ Retrying _}</span>
                    <span class="btn-xs failed status">{_ Failed _}</span>
                    <span class="btn-xs stopped status">{_ Stopped _}</span>
                </td>

                <td>
                    <a href="{{ m.zotonic_status.site_url[name] }}">{{ m.zotonic_status.site_url[name] }}</a>
                    <small>({{ name }})</small>
                </td>

                {% if m.acl.user == 1 %}
                    <td>
                        {% button
                            text=_"start"
                            class="start btn btn-default btn-xs"
                            title=_"Start the site."
                            postback={site_start site=name}
                            delegate=`controller_zotonic_status`
                        %}

                        {% button
                            text=_"stop"
                            class="stop btn btn-default btn-xs"
                            title=_"Stop the site."
                            action={confirm
                                text=[ _"Are you sure you want to stop the site:", " <b>", name|make_list, "</b> ?" ]
                                ok=_"stop"
                                is_danger
                                postback={site_stop site=name}
                                delegate=`controller_zotonic_status`
                            }
                        %}

                        {% button
                            text=_"restart"
                            class="restart btn btn-default btn-xs"
                            title=_"Restart the site, all users will be logged off."
                            action={confirm
                                text=[ _"Are you sure you want to restart the site:", " <b>", name|make_list, "</b> ?" ]
                                ok=_"restart"
                                is_danger
                                postback={site_restart site=name}
                                delegate=`controller_zotonic_status`
                            }
                        %}

                        {% button
                            text=_"flush"
                            class="flush btn btn-default btn-xs"
                            title=_"Flush and reload all settings, templates etc."
                            postback={site_flush site=name}
                            delegate=`controller_zotonic_status`
                        %}

                        {% button
                            text=_"admin"
                            class="admin btn btn-default btn-xs"
                            title=_"Visit the admin page for this site."
                            postback={site_admin site=name}
                            delegate=`controller_zotonic_status`
                        %}

                        {% all include "_z_status_button.tpl" %}
                    </td>
                {% endif %}
            </tr>
        {% endif %}
    {% endfor %}
</tbody>
</table>
