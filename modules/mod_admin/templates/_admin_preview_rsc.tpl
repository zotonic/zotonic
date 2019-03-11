<p class="meta">
    {{ id.category_id.title }}
    &mdash; {% include "_name.tpl" id=id.creator_id %}
    &mdash; {{ id.created|date:"Y-m-d H:i" }}
</p>
<h1>{{ id.title }}</h1>

{% if id.summary %}
    <p class="summary">
        {{ id.summary }}
    </p>
{% endif %}

{% if id.medium as medium %}
<div class="medium">
    {% media medium mediaclass="admin-media" %}
</div>
{% endif %}

{{ id.body }}

{% if not id.is_a.survey %}
    {% include "_blocks.tpl" %}
{% endif %}

{% if id.haspart as haspart %}
    <h2>{{ m.rsc.haspart.title }}</h2>
    <ul class="">
        {% for id in haspart %}
            {% if id.is_visible %}
                <li>
                    {{ id.title }}
                </li>
            {% endif %}
        {% endfor %}
    </ul>
{% endif %}
