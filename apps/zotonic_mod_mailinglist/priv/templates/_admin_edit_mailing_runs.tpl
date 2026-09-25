{% if id.is_a.mailinglist %}
    {% with m.mailinglist_run.list[id] as runs %}
        {% include "_admin_edit_mailing_runs_panel.tpl" runs=runs always %}
    {% endwith %}
{% else %}
    {% with m.mailinglist_run.page[id] as runs %}
        {% include "_admin_edit_mailing_runs_panel.tpl" runs=runs %}
    {% endwith %}
{% endif %}
