{% extends "page.tpl" %}

{% block title %}{_ Unsubscribe from mailing list _}{% endblock %}

{% block content %}

{% with m.mailinglist.confirm_key[q.confirm_key] as sub %}
{% if sub and m.rsc[sub.mailinglist_id].is_a.mailinglist %}
	<h1>{_ Unsubscribe from _} {{ m.rsc[sub.mailinglist_id].title|default:_"mailing list" }}</h1>
	<p>{_ Click <strong>unsubscribe</strong> to remove yourself from the mailing list. _}</p>
	<p id="confirm">
		{% button
		    text= [ _"Unsubscribe", " ", sub.email|escape ]
            class="btn btn-primary"
            action={mailinglist_unsubscribe id=sub.id
                    on_success={update target="confirm" text=_"Thank you. You are now unsubscribed."}
                    on_error={update target="confirm" text=_"Sorry, something went wrong. Please try again later."}}
        %}
    </p>

{% else %}
	<h1>{_ Sorry, can’t find your subscription _}</h1>
	<p>{_ The key in your link does not match any subscription. Either you already unsubscribed, or the mailing list has been deleted. _}</p>
	<p>{_ Our excuses for the inconvenience. _}</p>
{% endif %}
{% endwith %}

{% endblock %}
