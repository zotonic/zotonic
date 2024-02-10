{% extends "admin_base.tpl" %}

{% block title %}{_ Referrers to _} {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
{% with m.rsc[q.id].id as object_id %}
{% with m.search.paged[{referrers id=object_id page=q.page}] as result %}
<div class="admin-header">
    <h2 id="content-pager">{_ Referrers to _} <a href="{% url admin_edit_rsc id=object_id %}">{{ object_id.title }}</a></h2>

    {% ifequal result.total 0 %}
        <p>{_ There are no pages with a connection to the page _} “{{ object_id.title }}”</p>
    {% endifequal %}
</div>

<div>
    {% ifnotequal result.total 0 %}
        <table class="table do_adminLinkedTable">
            <thead>
                <tr>
                    <th width="30%">{_ Title _}</th>
                    <th width="15%">{_ Category _}</th>
                    <th width="15%">{_ Predicate _}</th>
                    <th width="15%">{_ Modified on _}</th>
                    <th width="25%">{_ Modified by _}</th>
                </tr>
            </thead>
            <tbody>
                {% for id, pred_id in result %}
                {% if id.is_visible %}
                <tr id="{{ #tr.id }}" class="{% if not m.rsc[id].is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                    <td><span {% include "_language_attrs.tpl" %}>{{ m.rsc[id].title|striptags|default:_"<em>Untitled</em>" }}</span></td>
                    <td>{{ m.rsc[m.rsc[id].category_id].title }}</td>
                    <td>{{ m.rsc[pred_id].title }}</td>
                    <td>{{ m.rsc[id].modified|date:_"d M Y, H:i" }}</td>
                    <td>
                        {{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}
                        <span class="pull-right buttons">
                            {% if id.page_url %}<a href="{{ id.page_url }}" class="btn btn-outline-secondary btn-sm">{_ view _}</a>{% endif %}
                            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-outline-secondary btn-sm">{_ edit _}</a>
                        </span>
                    </td>
                </tr>
                {% endif %}
                {% empty %}
                <tr>
                    <td colspan="5">
                        {_ No pages found. _}
                    </td>
                </tr>
                {% endfor %}
            </tbody>
        </table>
        {% pager result=result dispatch="admin_referrers" hide_single_page=1 id=object_id qargs %}
        <div class="text-muted clear-left">
            {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
            {% else %}{% trans "{n} items found." n=result.total %}
            {% endif %}
        </div>
    {% endifnotequal %}
</div>
{% endwith %}
{% endwith %}
{% endblock %}
