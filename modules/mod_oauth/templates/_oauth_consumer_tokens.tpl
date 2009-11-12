{% with m.oauth_app.info[id] as app %}
{% with m.oauth_app.tokens[id] as tokens %}

<h2>{{ app.application_title }}</h2>

{% if delete %}
{% if tokens %}
<p>The following users are still using this application. If you delete this application, you will revoke access and most likely their applications will break.</p>
{% endif %}
{% else %}
{% if tokens %}
<p>The following users are using this application:</p>
{% endif %}
{% endif %}

{% if tokens %}
<table width="100%">
    <tr>
        <th>User</th>
        <th>In use since</th>
        <th>Type</th>
    </tr>
    
    {% for token in tokens %}
    <tr>
        <td><a href="{{ m.rsc[token.user_id].page_url }}">{{ m.rsc[token.user_id].title }}</a></td>
        <td>{{ token.timestamp|date:"Y-d-m H:i" }}</td>
        <td>{{ token.token_type }}</td>
    </tr>
    {% endfor %}
{% else %}
<p>This token is not yet used by anybody.</p>
{% endif %}

{% if delete %}
    <p>Do you really want to delete this application?</p>
    <p>
        {% button text="Delete this application!" postback={confirm_del_app id=id} %}
    </p>
{% endif %}

{% endwith %}
{% endwith %}
