<h2>{_ Comments _}</h2>

{% with m.rsc[id].creator_id as creator_id %}
<ul id="comments-list" class="comments-list">
{% for comment in m.comment.rsc[id] %}
	{% include "_comments_comment.tpl" %}
{% endfor %}
</ul>
{% endwith %}

{% include "_comments_form.tpl" %}
