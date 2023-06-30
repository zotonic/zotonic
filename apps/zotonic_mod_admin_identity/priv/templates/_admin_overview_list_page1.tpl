{% if m.acl.use.mod_admin_identity %}
    {% if q.qs|match:".*@.*" %}
        {% if m.identity.lookup.email[q.qs] as idns %}
            <h2>{_ Identities with this email address _}</h2>

            <table class="table table-striped admin-table do_adminLinkedTable">
                <thead>
                    <tr>
                        <th width="26%">{_ Name _}</th>
                        <th width="20%"> {_ Email _}</th>
                        <th width="10%">{_ Category _}</th>
                        <th width="12%" class="hidden-xs">{_ Created on _}</th>
                        <th width="12%">{_ Modified on _}</th>
                        <th width="20%" class="hidden-xs">{_ Modified by _}</th>
                    </tr>
                </thead>

                <tbody>
                    {% for idn in idns %}
                    {% with idn.rsc_id as id %}
                        {% if id.is_visible %}
                        <tr class="{% if not m.rsc[id].is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                            <td>
                                {% include "_name.tpl" id=id %}
                            </td>
                            <td>
                                {{ id.email }}
                            </td>
                            <td>
                                {{ id.category_id.title }}
                            </td>
                            <td class="hidden-xs">{{ m.rsc[id].created|date:_"d M Y, H:i" }}</td>
                            <td>{{ m.rsc[id].modified|date:_"d M Y, H:i" }}</td>
                            <td>
                                <span class="hidden-xs">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
                                <span class="pull-right">
                                    {% if id.page_url %}<a href="{{ m.rsc[id].page_url }}" class="btn btn-xs btn-default">{_ view _}</a>{% endif %}
                                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-xs btn-default">{_ edit _}</a>
                                </span>
                            </td>
                        </tr>
                        {% endif %}
                    {% endwith %}
                    {% endfor %}
                </tbody>
            </table>
        {% endif %}
    {% endif %}
{% endif %}
