{% if m.acl.is_admin %}
{% with m.admin_status.listeners as listeners %}
    <div class="table-responsive">
        <table class="table table-striped">
            <thead>
                <tr>
                    <th scope="col">{_ Service _}</th>
                    <th scope="col">{_ Family _}</th>
                    <th scope="col">{_ Bind address _}</th>
                    <th scope="col">{_ Port _}</th>
                    <th scope="col">{_ Status _}</th>
                    <th scope="col" class="text-right">{_ Active _}</th>
                    <th scope="col" class="text-right">{_ Total _}</th>
                    <th scope="col" class="text-right">{_ Ranch limit _}</th>
                </tr>
            </thead>
            <tbody>
                {% for listener in listeners %}
                    <tr>
                        <th scope="row">{{ listener.service|escape }}</th>
                        <td>{% if listener.family == `inet6` %}IPv6{% else %}IPv4{% endif %}</td>
                        <td>
                            <code>{{ listener.ip|default:"—"|escape }}</code>
                            {% if listener.configured %}<span class="text-muted">({_ Configured _})</span>{% endif %}
                        </td>
                        <td>{{ listener.port|default_if_undefined:"—"|escape }}</td>
                        <td>
                            {% if listener.status == `running` %}
                                <span class="text-success">{_ Running _}</span>
                            {% elseif listener.status == `suspended` %}
                                <span class="text-warning">{_ Suspended _}</span>
                            {% elseif listener.status == `disabled` %}
                                <span class="text-muted">{_ Disabled _}</span>
                            {% elseif listener.status == `not_running` %}
                                <span class="text-danger">{_ Not running _}</span>
                            {% else %}
                                <span class="text-warning">{_ Unavailable _}</span>
                            {% endif %}
                        </td>
                        <td class="text-right">{{ listener.active_connections|default_if_undefined:"—"|escape }}</td>
                        <td class="text-right">{{ listener.all_connections|default_if_undefined:"—"|escape }}</td>
                        <td class="text-right">
                            {% if listener.max_connections == `infinity` %}{_ Unlimited _}{% else %}{{ listener.max_connections|default_if_undefined:"—"|escape }}{% endif %}
                        </td>
                    </tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
    <p>
        {% button class="btn btn-default" text=_"Refresh"
            action={update target="admin-status-listeners" template="_admin_status_listeners.tpl"}
        %}
        <span class="text-muted">{_ Updated _}: {{ now|date:"Y-m-d H:i:s T" }}</span>
    </p>
    <p class="help-block">
        {_ These listeners are shared by all sites on this Zotonic node. Running means the local listener is active; it does not confirm external reachability. _}<br>
        {_ The addresses 0.0.0.0 and :: mean all local IPv4 and IPv6 interfaces respectively. Configured bindings are shown when a listener is absent. _}<br>
        {_ Total connections counts currently tracked connections, not connections over time. The Ranch limit is the listener connection limit; SMTP also applies a separate session check. _}
    </p>
{% endwith %}
{% endif %}
