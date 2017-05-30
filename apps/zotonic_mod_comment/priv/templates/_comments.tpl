{% with m.comment.rsc[id] as comments %}
{% if comments %}

<h2>{_ Comments _}</h2>
{% with m.rsc[id].creator_id as creator_id %}
<ul id="comments-list" class="comments-list">
{% for comment in comments %}
   {% if comment.is_visible %}
      {% include "_comments_comment.tpl" %}
   {% endif %}
{% endfor %}
</ul>
{% endwith %}

{% endif %}
{% endwith %}

{% include "_comments_form.tpl" %}
