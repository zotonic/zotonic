{% extends "admin_base.tpl" %}

{% block title %}{_ Deleted pages _}{% endblock %}

{% block content %}

<div class="admin-header">

    <h2>{_ Deleted pages _}</h2>

    <p>
        {_ List of recently deleted pages and recover options via the resource revisions log. _}<br>
        {_ Only the texts and other properties of pages are backed up. _} {_ Connections and media items are not backed up and can’t be recovered. _}
    </p>
</div>

{% if m.acl.use.mod_backup %}
    {% with m.search.backup_deleted::%{ page: q.page } as result %}
        <table class="table table-striped do_adminLinkedTable">
            <thead>
                <tr>
                    <th width="25%">
                        {_ Title _}
                    </th>
                    <th width="15%">
                        {_ Category _}
                    </th>
                    <th width="15%">
                        {_ Deleted on _}
                    </th>
                    <th width="15%">
                        {_ Modified on _}
                    </th>
                    <th width="15%" class="hidden-xs">
                        {_ Deleted by _}
                    </th>
                    <th width="15%" class="hidden-xs">
                        {_ Created by _}
                    </th
                    <th>
                    </tr>
                </tr>
            </thead>
            <tbody>
                {% for r in result.result %}
                    <tr class="do_clickable" data-href="{% url admin_backup_revision id=r.rsc_id %}">
                        <td>
                            {% if r.data.title %}
                                {{ r.data.title }}
                            {% else %}
                                {{ r.rsc_id }}
                            {% endif %}

                            {% if r.new_id %}
                                <br>
                                {% if r.new_id.exists %}
                                    &rarr; <a href="{% url admin_edit_rsc id=r.new_id %}">{{ r.new_id.title|default:_"Untitled" }} <span class="text-muted">({{ r.new_id }})</span></a>
                                {% else %}
                                    &rarr; ({_ Deleted _}) <span class="text-muted">({{ r.new_id }})
                                {% endif %}
                            {% endif %}
                        </td>
                        <td>
                            {{ m.rsc[r.data.category_id].title|default:_"<i>Unknown</i>" }}
                        </td>
                        <td>
                            {{ r.created|date:"Y-m-d H:i" }}
                        </td>
                        <td>
                            {{ r.data.modified|date:"Y-m-d H:i" }}
                        </td>
                        <td>
                            {% if m.rsc[r.user_id].exists %}
                                {% include "_name.tpl" id=r.user_id %}
                            {% else %}
                                <span class="text-muted">{{ r.user_name|escape }}</span>
                            {% endif %}
                        </td>
                        <td>
                            {% include "_name.tpl" id=r.data.creator_id %}
                        </td>
                        <td>
                            <a href="{% url admin_backup_revision id=r.rsc_id %}" class="btn btn-xs btn-default">{_ Recover _}</a>
                        </td>
                    </tr>
                {% endfor %}
            </tbody>
        </table>

        {% pager result=result qargs %}
    {% endwith %}
{% else %}
    <div class="alert alert-info">{_ You need to have access rights to the backup module. _}</div>
{% endif %}

{% endblock %}
