<h3>{_ Send a new mailing _}</h3>
<p>{_ Select a mailing list, then choose language, recipients and timing. Previous results are preserved. _}</p>

<table class="table table-striped mailing-list-choices">
    <thead>
        <tr>
            <th>{_ Mailing list _}</th>
        </tr>
    </thead>
    <tbody>
    {% for title, mid in m.search[{all_bytitle cat="mailinglist" pagelen=1000}] %}
        {% if m.rsc[mid].is_editable or m.rsc[mid].name == "mailinglist_test" %}
            <tr>
                <td>
                    <a href="{% url admin_edit_rsc id=mid %}">{{ title }}</a>
                    {% if m.rsc[mid].summary as summary %}
                        <p class="text-muted">{{ summary }}</p>
                    {% endif %}
                    <div class="mailing-list-choice-action">
                        {% button class="btn btn-primary btn-sm"
                                  text=_"Configure mailing"
                                  action={dialog_mailing_page id=id list_id=mid}
                        %}
                    </div>
                </td>
            </tr>
        {% endif %}
    {% endfor %}
    </tbody>
</table>

<h3 id="mailing-history">{_ Mailing history for this page _}</h3>
{% include "_mailing_runs.tpl" runs=m.mailinglist_run.page[id] %}

<a href="{% url admin_mailings page_id=id %}">{_ View full mailing history _}</a>
