<div class="intro highlight">
    {{ m.rsc.page_home.summary|default:m.rsc.page_home.body|default:"<span class='label label-important'>Important</span> <i>Set the summary of page with name 'page_home'</i>" }}
    {% if m.rsc.page_home.is_editable %}<a class="btn btn-mini pull-right" href="{% url admin_edit_rsc id=m.rsc.page_home.id %}">Edit</a>{% endif %}
</div>

{% with m.rsc.main_mailinglist.id as mailinglist_id %}
{% if mailinglist_id.is_published %}
    {% mailinglist_subscribe id=mailinglist_id is_email_only %}
{% endif %}
{% endwith %}

{% include "_simple_list.tpl" list=m.search[{featured cat=`article` cat=`tweet` pagelen=10}] %}

