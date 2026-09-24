{% if m.acl.use.mod_mailinglist and (runs or always) %}
<div class="widget do_adminwidget">
    <div class="widget-header">{_ Mailing status _}<div class="widget-header-tools"></div></div>
    <div class="widget-content">{% for run in runs|slice:[,3] %}
        <p><a href="{% url admin_mailing_run run_id=run.id %}">{{ m.rsc[run.page_id].title }} → {{ m.rsc[run.mailinglist_id].title }}</a><br>
        {% if run.is_test %}{_ Test: _}{% endif %}{% include "_mailing_run_status.tpl" status=run.status type=run.type due=run.due %}
        · {{ run.stats.sent|default:0 }} {_ sent _}
        {% if run.stats.selected and run.status == "sending" or run.stats.selected and run.status == "retrying" %}
            <progress value="{{ run.stats.processed }}" max="{{ run.stats.selected }}" aria-label="{_ Mailing progress _}"></progress> {{ run.stats.percent }}%
        {% endif %}</p>
        {% empty %}<p>{_ No mailings yet. _}</p>{% endfor %}
        <a href="{% if id.is_a.mailinglist %}{% url admin_mailings list_id=id %}{% else %}{% url admin_mailings page_id=id %}{% endif %}">{_ All mailings _}</a></div>
</div>
{% endif %}
