{% extends "admin_base.tpl" %}

{% block title %}{_ WebSub subscriptions _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ WebSub subscriptions _}</h2>
    <p>{_ Subscriptions from others to local pages and our subscriptions to external pages. _}</p>
</div>
{% if m.acl.use.mod_admin_config %}
    {% with m.websub.subscriptions::%{type: q.type, rsc_id: q.rsc_id, page: q.page, hostname: q.hostname, status: q.status, errors: q.errors} as result %}
        <form method="get" action="{% url admin_websub %}" class="form-inline well">
            <div class="form-group">
                <label for="websub-type">{_ Subscription type _}</label>
                <select name="type" id="websub-type" class="form-control">
                    <option value="all">{_ All subscriptions _}</option>
                    <option value="export" {% if q.type == "export" %}selected{% endif %}>{_ Others subscribing to our pages _}</option>
                    <option value="import" {% if q.type == "import" %}selected{% endif %}>{_ Our subscriptions to external pages _}</option>
                </select>
            </div>
            <div class="form-group">
                <label for="websub-rsc-id">{_ Local resource ID _}</label>
                <input type="number" min="1" max="2147483647" name="rsc_id" id="websub-rsc-id" value="{{ q.rsc_id|escape }}" class="form-control">
            </div>
            <div class="form-group">
                <label for="websub-hostname">{_ External hostname _}</label>
                <input type="text" name="hostname" id="websub-hostname" value="{{ q.hostname|escape }}" maxlength="255" placeholder="example.com" class="form-control" aria-describedby="websub-hostname-help">
            </div>
            <div class="form-group">
                <label for="websub-status">{_ Status _}</label>
                <select name="status" id="websub-status" class="form-control">
                    <option value="all">{_ All statuses _}</option>
                    <option value="active" {% if q.status == "active" %}selected{% endif %}>{_ Active _}</option>
                    <option value="pending" {% if q.status == "pending" %}selected{% endif %}>{_ Pending confirmation _}</option>
                    <option value="expired" {% if q.status == "expired" %}selected{% endif %}>{_ Expired _}</option>
                    <option value="stopped" {% if q.status == "stopped" %}selected{% endif %}>{_ Stopped _}</option>
                </select>
            </div>
            <div class="form-group">
                <label for="websub-errors">{_ Errors _}</label>
                <select name="errors" id="websub-errors" class="form-control">
                    <option value="all">{_ All _}</option>
                    <option value="yes" {% if q.errors == "yes" %}selected{% endif %}>{_ With errors _}</option>
                    <option value="no" {% if q.errors == "no" %}selected{% endif %}>{_ Without errors _}</option>
                </select>
            </div>
            <button type="submit" class="btn btn-primary">{_ Filter _}</button>
            <a href="{% url admin_websub %}" class="btn btn-default">{_ Reset _}</a>
            <p class="help-block" id="websub-hostname-help">{_ Enter the exact hostname, without a scheme, port, or path. _}</p>
        </form>
        {% if result.is_invalid %}
            <p class="alert alert-warning">{_ Select a valid subscription type, resource ID, hostname, status, errors, and page number. _}</p>
        {% else %}
            <p class="help-block">{_ Expired and stopped subscriptions are included. Renewals can temporarily have two active subscriptions. Remote hosts are shown without callback URLs or credentials. _}</p>
            <div class="table-responsive">
                <table class="table table-striped">
                    <thead><tr>
                        <th>{_ Type _}</th><th>{_ Local page _}</th><th>{_ Remote host _}</th>
                        <th>{_ Status _}</th><th>{_ Lease expires _}</th><th>{_ Last activity _}</th>
                    </tr></thead>
                    <tbody>
                    {% for sub in result.rows %}
                        <tr>
                            <td>{% if sub.type == "export" %}{_ Incoming subscriber _}{% else %}{_ Outgoing subscription _}{% endif %}</td>
                            <td>
                                {% if sub.local_rsc_id %}
                                    {% if m.rsc[sub.local_rsc_id].is_editable %}
                                        <a href="{% url admin_edit_rsc id=sub.local_rsc_id %}">{{ m.rsc[sub.local_rsc_id].title|default:_"Untitled" }}</a>
                                    {% else %}
                                        {{ m.rsc[sub.local_rsc_id].title|default:_"Untitled" }}
                                    {% endif %}
                                    <span class="text-muted">({{ sub.local_rsc_id|escape }})</span>
                                {% else %}{_ Deleted page _}{% endif %}
                            </td>
                            <td>{{ sub.peer_host|escape }}</td>
                            <td>
                                {% if sub.status == "active" %}{_ Active _}
                                {% elseif sub.status == "stopped" %}{_ Stopped _}
                                {% elseif sub.status == "expired" %}{_ Expired _}
                                {% else %}{_ Pending confirmation _}{% endif %}
                                {% if sub.has_error %}<span class="label label-warning">{_ Error _}</span>{% endif %}
                            </td>
                            <td>{{ sub.lease|date:"Y-m-d H:i"|default:"—" }}</td>
                            <td>{{ sub.last_activity|date:"Y-m-d H:i"|default:"—" }}</td>
                        </tr>
                    {% empty %}
                        <tr><td colspan="6">{_ No subscriptions found. _}</td></tr>
                    {% endfor %}
                    </tbody>
                </table>
            </div>
            <nav aria-label="{_ Subscription pages _}">
                <ul class="pager">
                    {% if result.page > 1 %}<li><a href="{% url admin_websub type=result.type rsc_id=result.rsc_id hostname=result.hostname status=result.status errors=result.errors page=result.previous_page %}">{_ Previous _}</a></li>{% endif %}
                    {% if result.has_next %}<li><a href="{% url admin_websub type=result.type rsc_id=result.rsc_id hostname=result.hostname status=result.status errors=result.errors page=result.next_page %}">{_ Next _}</a></li>{% endif %}
                </ul>
            </nav>
        {% endif %}
    {% endwith %}
{% else %}
    <p class="alert alert-danger">{_ You are not allowed to view subscriptions. _}</p>
{% endif %}
{% endblock %}
