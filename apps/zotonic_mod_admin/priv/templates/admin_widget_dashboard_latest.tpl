{% extends "admin_widget_dashboard.tpl" %}

{#
    Display latest modified rscs

    Required arguments:
    cat= rsc category
    headline= block caption

    Optional:
    last= true|false default false
    pagelen= page length, default 5
    show_date= true|false default false

#}

{% block widget_headline %}
    {{ headline }}
    {% if media %}
        {% button class="btn btn-default btn-xs pull-right" action={redirect dispatch="admin_media"} text=_"show all"%}
    {% else %}
        {% button class="btn btn-default btn-xs pull-right" action={redirect dispatch="admin_overview_rsc" qcat=cat} text=_"show all"%}
    {% endif %}
{% endblock %}

{% block widget_class %}{% if last %}last{% endif %}{% endblock %}

{% block widget_content %}
    {% with m.search[{latest cat=cat pagelen=pagelen|default:5}] as latest %}
        {% if latest %}
            <table class="table do_adminLinkedTable">
                <thead>
                    <tr>
                        {% if show_date %}
                            <th width="40%">{_ Title _}</th>
                            <th width="30%">{_ Date _}</th>
                            <th width="30%">{_ Category _}</th>
                        {% else %}
                            <th width="55%">{_ Title _}</th>
                            <th width="45%">{_ Category _}</th>
                        {% endif %}
                    </tr>
                </thead>
                <tbody>
                    {% for id in latest %}
                        {% if m.rsc[id].is_visible %}
                            <tr class="{% if not m.rsc[id].is_published %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
                                <td>
                                    {% if media %}
                                        <div class="admin-list-thumb">
                                        {% image id mediaclass="admin-list-dashboard" class="thumb" %}
                                    {% endif %}
                                    <span>{{ (m.rsc[id].title|striptags|truncate:50)|default:"<em>untitled</em>" }}</span>
                                    {% if media %}
                                        </div>
                                    {% endif %}
                                </td>

                                {% if show_date %}
                                    <td>
                                        {{ id.modified|date:"j F Y - H:i:s" }}
                                    </td>
                                    <td>
                                        {{ id.category_id.short_title|default:id.category_id.title }}
                                        <span class="pull-right buttons">
                                            {% if id.page_url %}<a href="{{ id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>{% endif %}
                                            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                                        </span>
                                    </td>
                                {% else %}
                                    <td>
                                        {{ id.category_id.short_title|default:id.category_id.title }}
                                        <span class="pull-right buttons">
                                            {% if id.page_url %}<a href="{{ id.page_url }}" class="btn btn-default btn-xs">{_ view _}</a>{% endif %}
                                            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                                        </span>
                                    </td>
                                {% endif %}
                            </tr>
                        {% endif %}
                    {% endfor %}
                </tbody>
            </table>
            {% else %}
                <div class="widget-content widget-content-full"><div class="table-like"><span class="text-muted">{_ No items _}</span></div></div>
            {% endif %}
    {% endwith %}
{% endblock %}
