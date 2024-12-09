{% if m.acl.use.mod_mailinglist or id.is_editable or id == m.acl.user %}
    {% if m.mailinglist.subscriptions[id] as subs %}
    <div id="rsc-mailinglist-subs" class="widget do_adminwidget" data-adminwidget='{ "minifiedOnInit":true, "minifier":true }'>
        <div class="widget-header">{_ Mailinglist subscriptions _}<div class="widget-header-tools"></div></div>

        <div class="widget-content">
            {% for sub in subs %}
                <div class="col-md-4" id="msub-{{ forloop.counter }}">
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
                        {% if sub.email %}
                            <small>{_ Subscribed with _} <b>{{ sub.email|escape }}</b></small>
                        {% endif %}
                    </p>
                </div>
            {% endfor %}
        </div>
    </div>
    {% endif %}
{% endif %}
