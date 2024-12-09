{% extends "page.tpl" %}

{% block title %}{_ Manage your mailing list subscriptions _}{% endblock %}

{% block content %}
{% with q.key|if
        :(q.key|mailinglist_recipient_key_decode)
        :%{
            user_id: m.acl.user,
            is_mailing_opt_out: m.acl.user.is_mailing_opt_out,
            subscriptions: m.mailinglist.subscriptions[m.acl.user]
         }
    as recipient
%}

    <h1>{_ Your mailing list subscriptions _}</h1>

    {% if not recipient %}
        <p>{_ Use the link in your email to manage your subscriptions. _}</p>
    {% elseif recipient.rsc_id %}
        <p>{_ Change subscriptions connected to your account._}</p>
    {% else %}
        <p>
            {% trans "Here you can change subscriptions connected to your email address <b>{email}</b>."
                     email=recipient.email|escape
            %}
        </p>
    {% endif %}

    {% if recipient %}
        {% if recipient.rsc_id %}
            {% wire id=#optout
                    type="submit"
                    postback={mailinglist_optout id=recipient.rsc_id}
                    delegate="mod_mailinglist"
                    action={script script="
                        if ($('#is_mailing_opt_out').is(':checked')) {
                            $('#subscriptions-wrapper').fadeOut();
                        } else {
                            $('#subscriptions-wrapper').fadeIn();
                        }
                    "}
            %}
            <h2>{_ Opt-out _}</h2>
            <form id="{{ #optout }}" class="form-inline" action="postback">
                <p>
                    {_ You can opt-out of all current and future mailings using your account on this website. _}
                    {% if recipient.rsc_id %}
                       <br>
                       {_ If you opt-out then it is still possible to subscribe to mailing lists using an e-mail address (instead of your account). _}
                    {% endif %}
                </p>
                <p>
                    <label class="checkbox">
                        <input type="checkbox" id="is_mailing_opt_out" name="is_mailing_opt_out" value="1" {% if recipient.is_mailing_opt_out %}checked{% endif %}>
                        {_ Opt-out of all mailing lists coupled to my account _}
                    </label>
                </p>
                <p>
                    <button type="submit" class="btn btn-primary">{_ Save _}</button>
                </p>
            </form>
        {% endif %}

        <div id="subscriptions-wrapper" {% if recipient.is_mailing_opt_out %}style="display: none"{% endif %}>
            {% if recipient.subscriptions %}
                <h2>{_ Your subscriptions _}</h2>

                <p>{_ You are subscribed to the following mailing lists. _}</p>
                <ul>
                    {% for sub in recipient.subscriptions %}
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
                                            recipient=recipient.rsc_id|default:recipient.email
                                            on_success={update
                                                target="msub-confirm-"++forloop.counter
                                                text=_"<b>Thank you.</b> You are now unsubscribed."}
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
            </div>
        {% else %}
            <h2>{_ Your subscriptions _}</h2>
            <p>{_ You are not subscribed to any mailing lists. _}</p>
        {% endif %}
    {% endif %}

{% endwith %}
{% endblock %}
