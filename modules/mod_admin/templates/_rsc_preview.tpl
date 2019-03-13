<p class="meta text-muted">
    <span class="pull-right">{{ id.category_id.title }}</span>
    {_ Created by _} <b>{% include "_name.tpl" id=id.creator_id %}</b>
    {% if id.modifier_id and id.modifier_id != id.creator_id %}
        {_ modified by _}
        <b>{% include "_name.tpl" id=id.modifier_id %}</b>
    {% endif %}
    <br>
    {_ Created on _} <b>{{ id.created|date:_"Y-m-d H:i" }}</b>
    {% if id.content_group_id %}
        <span>{_ in _}</span>
        <b>{{ id.content_group_id.title }}</b>
    {% endif %}
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
