{% extends "admin_base.tpl" %}

{% block title %}{_ Merge _} “{{ m.rsc[id].title }}”{% endblock %}

{% block bodyclass %}admin-page{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Find page to merge with: _} “{{ id.title }}”</h2>

        <p>
            {_ Merge two pages together. You can select with which page to merge and which page will remain after the merge. _}
        </p>
    </div>

    {% if id.is_editable %}
        {% include "_merge_find.tpl" 
            tab=#tab
            text=id.title
            delegate=`mod_admin_merge`
            cat=id.category_id
        %}
    {% else %}
        <div class="alert alert-warning" role="alert">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
             {_ You are not allowed to edit this page. _}
        </div>
    {% endif %}
{% endblock %}
