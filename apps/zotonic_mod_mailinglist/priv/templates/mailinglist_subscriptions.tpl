{% extends "page.tpl" %}

{% block title %}{_ Manage your mailing list subscriptions _}{% endblock %}

{% block content %}
{% with q.key|mailinglist_recipient_key_decode|default:m.acl.user as recipient %}

    <h1>{_ Your mailing list subscriptions _}</h1>

    {% if recipient|is_number %}
        <p>{_ Here you can change subscriptions connected to your account._}</p>
    {% else %}
        <p>{_ Here you can change subscriptions connected to your email address. _}</p>
    {% endif %}

    {% if not q.key and not m.acl.user %}
        <p>{_ Use the link in your email to manage your subscriptions. _}</p>
    {% else %}
        {% if m.mailinglist.subscriptions[recipient] as subs %}
            <h2>{_ Your subscriptions _}</h2>

            <ul>
                {% for sub in subs %}
                    <li id="msub-{{ forloop.counter }}">
                        <p><b>{{ sub.title }}</b></p>

                        {% if sub.summary %}
                            <p>{{ sub.summary }}</p>
                        {% endif %}

                        <p id="msub-confirm-{{ forloop.counter }}">
                            {% button
                                    text=_"Unsubscribe"
                                    class="btn btn-primary"
                                    postback={mailinglist_unsubscribe
                                        mailinglist_id=sub.mailinglist_id
                                        recipient_id=sub.recipient_id
                                        rsc_id=sub.rsc_id
                                        recipient=recipient
                                        on_success={update
                                            target="msub-confirm-"++forloop.counter
                                            text=_"Thank you. You are now unsubscribed."}
                                        on_error={update
                                            target="msub-confirm-"++forloop.counter
                                            text=_"Sorry, something went wrong. Please try again later."}
                                    }
                                    delegate="mod_mailinglist"
                            %}

                            {% if sub.email %}
                                <br><small>{_ Subscribed with _} <b>{{ sub.email|escape }}</b></small>
                            {% endif %}
                        </p>
                    </li>
                {% endfor %}
            </ul>
        {% elseif q.key %}
            <p>{_ Sorry, can’t find your subscription. _}</p>
            <p>{_ The key in your link does not match any subscription. Either you already unsubscribed, or the mailing list has been deleted. _}</p>
            <p>{_ Our excuses for the inconvenience. _}</p>
        {% else %}
            <p>{_ No subscriptions found. _}</p>
        {% endif %}

        {% if recipient|is_number %}
            {% wire id=#optout
                    type="submit"
                    postback={mailinglist_optout id=recipient}
                    delegate="mod_mailinglist"
            %}
            <h2>{_ Opt-out _}</h2>
            <form id="{{ #optout }}" class="form-inline" action="postback">
                <p>
                    {_ You can opt-out of all current and future mailings using your account on this website. _}<br>
                    {_ Note that it is still possible to subscribe by email-address to mailing lists. _}
                </p>
                <p>
                    <label class="checkbox">
                        <input type="checkbox" name="is_mailing_opt_out" value="1" {% if m.rsc[recipient].is_mailing_opt_out %}checked{% endif %}>
                        {_ Opt-out of all mailing lists coupled to my account _}
                    </label>
                </p>
                <p>
                    <button type="submit" class="btn btn-primary">{_ Save _}</button>
                </p>
            </form>
        {% endif %}
    {% endif %}
{% endwith %}
{% endblock %}
