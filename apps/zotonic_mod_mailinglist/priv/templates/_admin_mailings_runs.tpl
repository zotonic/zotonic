{% with m.mailinglist_run::%{
        status: filter_status,
        language: filter_language,
        page_id: filter_page,
        list_id: filter_list,
        offset: filter_offset
    } as runs
%}
    {% include "_mailing_runs.tpl" runs=runs %}
    {% if runs|length == 200 %}
        {% with filter_offset|default:0|to_integer + 200 as next_offset %}
            <a class="btn btn-default" href="{% url admin_mailings offset=next_offset status=filter_status language=filter_language page_id=filter_page list_id=filter_list %}"> {_ Next page _}</a>
        {% endwith %}
    {% endif %}
    {% if filter_offset %}
        <a class="btn btn-default" href="{% url admin_mailings status=filter_status language=filter_language page_id=filter_page list_id=filter_list %}">{_ First page _}</a>
    {% endif %}
{% endwith %}

<p class="help-block">{_ Showing up to 200 runs per page, with active runs first. Sent means accepted by the mail server; later bounces can still change the results. _}</p>
