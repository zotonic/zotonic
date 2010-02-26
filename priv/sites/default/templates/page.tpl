{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}
<div class="zp-100">
    <h1>{{ m.rsc[id].title }}</h1>

    <p class="summary">
        {{ m.rsc[id].summary }}
    </p>

    {% if m.rsc[id].medium %}<p>{% image id width=400 class="block" %}</p>{% endif %}

    {{ m.rsc[id].body|show_media }}
</div>

{% block below_body %}{% endblock %}

{% endblock %}
