{% if m.acl.use.mod_mailinglist %}
    {% with m.rsc[q.qs].id|default:q.qs as qkey %}
        {% if m.mailinglist.subscriptions[qkey] as subs %}
            <h2>{_ Mailinglist subscriptions _}</h2>

            <div class="row">
                {% for sub in subs %}
                    <div class="col-md-3" id="msub-{{ forloop.counter }}">
                        <div class="well">
                            <p><b>{{ sub.title }}</b></p>

                            {% if sub.summary %}
                                <p>{{ sub.summary }}</p>
                            {% endif %}

                            <p id="msub-confirm-{{ forloop.counter }}">
                                {% button
                                        text=_"Unsubscribe"
                                        class="btn btn-primary"
                                        action={confirm
                                            text=_"Do you want to remove this subscription?"
                                            ok=_"Unsubscribe"
                                            is_danger
                                            postback={mailinglist_unsubscribe
                                                mailinglist_id=sub.mailinglist_id
                                                recipient_id=sub.recipient_id
                                                rsc_id=sub.rsc_id
                                                recipient=recipient.rsc_id|default:recipient.email
                                                on_success={update
                                                    target="msub-confirm-"++forloop.counter
                                                    text=_"Removed subscription."}
                                                on_error={update
                                                    target="msub-confirm-"++forloop.counter
                                                    text=_"Sorry, something went wrong. Please try again later."}
                                            }
                                            delegate="mod_mailinglist"
                                        }
                                %}
                            </p>
                            <p>
                                {% if sub.rsc_id %}
                                    <small>
                                        <b><a href="{% url admin_edit_rsc id=sub.rsc_id %}">{% include "_name.tpl" id=sub.rsc_id %}</a></b>
                                    </small><br>
                                {% endif %}
                                {% if sub.email %}
                                    <small>{_ Subscribed with _} <b>{{ sub.email|escape }}</b></small>
                                {% endif %}
                            </p>
                        </div>
                    </div>
                {% endfor %}
            </div>
        {% endif %}
    {% endwith %}
{% endif %}
