{% extends "admin_base.tpl" %}

{% block title %}{_ Merge _} “{{ id.title }}” &amp; “{{ m.rsc[q.id2].title }}”{% endblock %}

{% block bodyclass %}admin-page{% endblock %}

{% block content %}
{% with m.rsc[q.id2].id as id2 %}
    <div class="admin-header">
        <h2>{_ Merge Pages _}</h2>
        <p>
            {_ Select the winner from two pages. The loser will get merged into the winner. Only the winner remains. _}
        </p>
    </div>

    {% if id.is_editable and id2.is_editable %}
        <h3>{_ Compare and select _}</h3>

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
            </thead>
            <tbody>
            {% with m.identity[id].username,
                    m.identity[id2].username as username1, username2 %}
                {% if username1 or username2 %}
                    <tr>
                        <th>{_ Username _}</th>
                        <td>{{ username1|escape }}</td>
                        <td>{{ username2|escape }}</td>
                    </tr>
                {% endif %}
            {% endwith %}
            {% for prop, p1, p2 in id|admin_merge_diff:id2 %}
                {% if not prop|member:[`id`, `creator_id`, `modifier_id`, `modified`, `created`, `summary_html`, `version`] %}
                    <tr>
                        <th>{{ prop }}</th>
                        <td>{{ p1|linebreaksbr }}</td>
                        <td>{{ p2|linebreaksbr }}</td>
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
                        <button id="{{ #merge1 }}" class="btn btn-primary">{_ Select Left _}</button>
                    </th>
                    <th width="40%">
                        <button id="{{ #merge2 }}" class="btn btn-primary">{_ Select Right _}</button>
                    </th>
                </tr>
            </foot>
        </table>

        <p><br/></p>

        {% wire id=#merge1 
                action={confirm
                            title=_"Confirm merge"
                            text=[
                                    _"Winner:", " <b>", id.title, "</b><br/>",
                                    _"Loser:", " <b><del>", id2.title, "</del></b><br/><br/>",
                                    _"The winner will stay, the loser will be deleted and replaced with the winner.",
                                    "<br/><br/>",
                                    _"This cannot be undone."
                                 ]
                            ok=_"Merge"
                            postback={merge winner_id=id loser_id=id2} 
                            delegate=`mod_admin_merge`}
        %}
        {% wire id=#merge2
                action={confirm
                            title=_"Confirm merge"
                            text=[
                                    _"Winner:", " <b>", id2.title, "</b><br/>",
                                    _"Loser:", " <b><del>", id.title, "</del></b><br/><br/>",
                                    _"The winner will stay, the loser will be deleted and replaced with the winner.",
                                    "<br/><br/>",
                                    _"This cannot be undone."
                                 ]
                            ok=_"Merge"
                            postback={merge winner_id=id2 loser_id=id} 
                            delegate=`mod_admin_merge`}
        %}
    {% else %}
        <div class="alert alert-warning" role="alert">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
             {_ You are not allowed to edit this page. _}
        </div>
    {% endif %}
{% endwith %}
{% endblock %}
