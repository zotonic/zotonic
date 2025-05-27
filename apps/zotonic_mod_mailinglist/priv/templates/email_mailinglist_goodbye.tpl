{% extends "email_base.tpl" %}

{% block title %}{_ You are now unsubscribed from _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
{% include "_email_mailinglist_hello.tpl" %}

<p>
    {% trans "You are now unsubscribed from the mailing list <b>{title}</b>. Hope to see you again."
            title=m.rsc[list_id].title
    %}
</p>

{{ m.rsc[list_id].subscription_info_html|show_media }}

{% endblock %}
