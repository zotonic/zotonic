{% extends "admin_base.tpl" %}

{% block title %}{_ Mailings _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Mailings _}</h2>
        <p>{_ Follow active mailings and inspect previous results. To send a page, use Send mailing on its edit page. _}</p>
        <a href="{% url admin_mailinglist %}" class="btn btn-default">{_ Manage mailing lists _}</a>
    </div>
    {% if m.rsc[q.page_id].id as filtered_page %}
        <p>
            {_ Mailings for: _} <strong>{{ m.rsc[filtered_page].title }}</strong>
            <a href="{% url admin_mailings %}">{_ Show all pages _}</a><
        /p>
    {% endif %}
    <form method="get" class="form-inline">
        {% if m.rsc[q.page_id].id as page_id %}
            <input type="hidden" name="page_id" value="{{ page_id }}">
        {% endif %}
        <label>
            {_ Status _}
            <select name="status" class="form-control">
                <option value="">{_ All statuses _}</option>
                {% for state in [
                        "scheduled","preparing","sending","retrying","interrupted",
                        "completed","completed_errors","empty","failed","cancelled"
                    ]
                %}
                    <option value="{{ state }}" {% if q.status == state %}selected{% endif %}>
                        {% include "_mailing_run_status.tpl" status=state %}
                    </option>
                {% endfor %}
            </select>
        </label>
        <label>
            {_ Language _}
            <select name="language" class="form-control">
                <option value="">{_ All languages _}</option>
                {% for code,props in m.translation.language_list_enabled %}
                    <option value="{{ code|escape }}" {% if q.language == code %}selected{% endif %}>
                        {{ props.name|escape }}
                    </option>
                {% endfor %}
            </select>
        </label>
        <label>{_ Mailing list _}
            <select name="list_id" class="form-control">
                <option value="">{_ All lists _}</option>
                {% for title,mid in m.search[{all_bytitle cat="mailinglist" pagelen=1000}] %}
                    {% if m.rsc[mid].is_editable %}
                        <option value="{{ mid }}" {% if q.list_id|to_integer == mid %}selected{% endif %}>
                            {{ title }}
                        </option>
                    {% endif %}
                {% endfor %}
            </select>
        </label>
        <button type="submit" class="btn btn-default">{_ Filter _}</button>
    </form>
    {% live topic="bridge/origin/model/mailinglist/event/+/runs"
            template="_admin_mailings_runs.tpl"
            filter_status=q.status
            filter_language=q.language
            filter_page=q.page_id
            filter_list=q.list_id
            filter_offset=q.offset
    %}
{% endblock %}
