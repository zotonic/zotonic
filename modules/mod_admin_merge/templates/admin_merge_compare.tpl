{% extends "admin_base.tpl" %}

{% block title %}{_ Merge _} “{{ id.title }}” &amp; “{{ m.rsc[q.id2].title }}”{% endblock %}

{% block bodyclass %}admin-page{% endblock %}

{% block content %}
{% with m.rsc[q.id2].id as id2 %}
{% with
    id.is_editable,
    id2.is_editable,
    id.is_editable and id != 1,
    id2.is_editable and id2 != 1
    as
    is_editable1,
    is_editable2,
    is_deletable1,
    is_deletable2
%}
{% with
    is_editable1 and is_deletable2,
    is_editable2 and is_deletable1
    as
    selectable1,
    selectable2
%}
    <div class="admin-header">
        <h2>{_ Merge Pages _}</h2>
        <p>
            {_ Select the winner from two pages. The loser will get merged into the winner. Only the winner remains. _}
        </p>
    </div>

    <h3>{_ Select the winning page _}</h3>

    <table id="merge-diffs" class="table">
        <thead>
            <tr>
                <th></th>
                <th width="40%">
                    <h4>
                        {_ Left _}
                    </h4>
                    <a href="{% url admin_edit_rsc id=id %}" title="{_ Edit page _}">{{ id.title }}</a>
                </th>
                <th width="40%">
                     <h4>
                        {_ Right _}
                    </h4>
                    <a href="{% url admin_edit_rsc id=id2 %}" title="{_ Edit page _}">{{ id2.title }}</a>
                </th>
            </tr>
            {% if not selectable1 or not selectable2 %}
                <tr>
                    <td></td>
                    <td>
                        {% if not is_editable1 %}
                            <span class="form-field-error">{_ You are not allowed to edit this page _}</span>
                        {% endif %}
                        {% if not selectable1 %}
                            <span class="form-field-error">{_ You are not allowed to select this page and thereby deleting page Right. _}</span>
                        {% endif %}
                    </td>
                    <td>
                        {% if not is_editable1 %}
                            <span class="form-field-error">{_ You are not allowed to edit this page _}</span>
                        {% endif %}
                        {% if not selectable2 %}
                            <span class="form-field-error">{_ You are not allowed to select this page and thereby deleting page Left. _}</span>
                        {% endif %}
                    </td>
                </tr>
            {% endif %}
        </thead>
        <tbody>
        {% with m.identity[id].username,
                m.identity[id2].username as username1, username2 %}
            {% if username1 or username2 %}
                <tr>
                    <th>{_ Username _}</th>
                    <td class="{% if not selectable1 %}unpublished{% endif %}">{{ username1|escape }}</td>
                    <td class="{% if not selectable2 %}unpublished{% endif %}">{{ username2|escape }}</td>
                </tr>
            {% endif %}
        {% endwith %}
        {% for prop, p1, p2 in id|admin_merge_diff:id2 %}
            {% if not prop|member:[`id`, `creator_id`, `modifier_id`, `modified`, `created`, `summary_html`, `version`] %}
                <tr>
                    <th>{{ prop }}</th>
                    <td class="{% if not selectable1 %}unpublished{% endif %}">{{ p1|linebreaksbr }}</td>
                    <td class="{% if not selectable2 %}unpublished{% endif %}">{{ p2|linebreaksbr }}</td>
                </tr>
            {% endif %}
        {% endfor %}
        {% if id.medium or id2.medium %}
            <tr>
                <th>{_ Media _}</th>
                <td>
                    {% media id mediaclass="admin-rsc-edge-media" %}
                </td>
                <td>
                    {% media id2 mediaclass="admin-rsc-edge-media" %}
                </td>
            </tr>
        {% endif %}
        </tbody>
        <tfoot>
            <tr>
                <th></th>
                <th width="40%">
                    <button id="{{ #merge1 }}" class="btn btn-primary"{% if not selectable1 %} disabled="disabled"{% endif %}>{_ Select Left _}</button>
                </th>
                <th width="40%">
                    <button id="{{ #merge2 }}" class="btn btn-primary"{% if not selectable2 %} disabled="disabled"{% endif %}>{_ Select Right _}</button>
                </th>
            </tr>
        </foot>
    </table>

    <p><br/></p>

    {% wire
        id=#merge1
        action={dialog_open
            title=_"Confirm merge"
            template="_confirm_merge.tpl"
            winner_id=id
            loser_id=id2
            left=1
        }
    %}
    {% wire
        id=#merge2
        action={dialog_open
            title=_"Confirm merge"
            template="_confirm_merge.tpl"
            winner_id=id2
            loser_id=id
            right=1
        }
    %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endblock %}
