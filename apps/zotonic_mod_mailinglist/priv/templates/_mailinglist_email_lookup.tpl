{% if m.mailinglist.email_subscriptions[email] as lookup %}
    <section class="widget" aria-labelledby="email-subscriptions-title">
        <div class="widget-header">
            <h3 id="email-subscriptions-title">{_ Subscriptions _}: {{ lookup.email|escape }}</h3>
        </div>
        <div class="widget-content">
            <p>
                <strong>{_ Email status _}:</strong>
                {% if lookup.email_status as status %}
                    {% if status.is_blocked %}
                        <span class="text-danger">{_ Blocked _}</span>
                    {% elseif not status.is_ok_to_send %}
                        <span class="text-danger">{_ Sending is suppressed after delivery failures. _}</span>
                    {% elseif not status.is_valid %}
                        <span class="text-warning">{_ Delivery problems have been recorded; sending is still allowed. _}</span>
                    {% else %}
                        {_ Sending is allowed. _}
                    {% endif %}
                    {% include "_mailinglist_email_status_flag.tpl" email=lookup.email %}
                {% else %}
                    <span class="text-muted">{_ You do not have permission to view the email status. _}</span>
                {% endif %}
            </p>
            <p><a href="{% url admin_mailings email=lookup.email %}">{_ Find recent mailings to this address _}</a></p>
            <table class="table table-striped">
                <thead><tr>
                    <th>{_ Mailing list _}</th>
                    <th>{_ Subscriber _}</th>
                    <th>{_ Subscription status _}</th>
                </tr></thead>
                <tbody>
                {% for subscription in lookup.subscriptions %}
                    <tr>
                        <td><a href="{% url admin_mailinglist_recipients id=subscription.mailinglist_id %}">{{ m.rsc[subscription.mailinglist_id].title }}</a></td>
                        <td>
                            {% if subscription.rsc_id %}
                                <a href="{% url admin_edit_rsc id=subscription.rsc_id %}">{{ m.rsc[subscription.rsc_id].title|default:subscription.rsc_id }}</a>
                                <small>{_ Primary email address _}</small>
                            {% else %}
                                {_ Email-only subscription _}
                            {% endif %}
                        </td>
                        <td>
                            {% if subscription.is_enabled %}{_ Subscribed _}{% else %}{_ Unsubscribed _}{% endif %}
                            {% if subscription.is_opt_out %}<br><small>{_ This resource has opted out of mailings. _}</small>{% endif %}
                        </td>
                    </tr>
                {% empty %}
                    <tr><td colspan="3" class="text-muted">{_ No explicit subscriptions found for this email address. _}</td></tr>
                {% endfor %}
                </tbody>
            </table>
            <p class="help-block">{_ Query-based subscriptions are not included. Disabled email-only subscriptions are retained for three months. _}</p>
            {% if lookup.resources %}
                <h4>{_ Matching resources _}</h4>
                <div class="row">
                    {% for resource in lookup.resources %}
                        {% with resource.rsc_id as id %}
                            <div class="col-sm-6 col-md-4">
                                <a class="thumbnail{% if id.depiction %} z-image-thumbnail{% endif %}" href="{% url admin_edit_rsc id=resource.rsc_id %}">
                                    {% if id.depiction as depiction %}
                                        <div class="z-thumbnail-image">
                                            {% image depiction mediaclass="admin-list-overview" class="thumb" %}
                                        </div>
                                    {% endif %}
                                    <div class="z-thumbnail-text">
                                        <h6>{{ id.category_id.title }}</h6>
                                        <h5>{{ id.title|default:id.short_title|default:_"Untitled"|truncate:45 }}</h5>
                                        <p>
                                            {% if resource.is_opt_out %}
                                                <span class="label label-warning">{_ Opted out of mailings _}</span>
                                            {% else %}
                                                <span class="label label-success">{_ Not opted out of mailings _}</span>
                                            {% endif %}
                                        </p>
                                    </div>
                                </a>
                            </div>
                        {% endwith %}
                    {% endfor %}
                </div>
            {% endif %}
        </div>
    </section>
{% else %}
    <p class="alert alert-warning">{_ Enter a valid email address to look up subscriptions. _}</p>
{% endif %}
