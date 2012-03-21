{% with m.oauth_app.info[id] as app %}
{% with m.oauth_app.tokens[id] as tokens %}

<h2>{{ app.application_title }}</h2>

{% if delete %}
{% if tokens %}
<p>{_ The following users are still using this application. If you delete this application, you will revoke access and most likely their applications will break. _}</p>
{% endif %}
{% else %}
{% if tokens %}
<p>{_ The following users are using this application: _}</p>
{% endif %}
{% endif %}

{% if tokens %}
<table class="table" width="100%">
    <thead>
        <tr>
            <th>{_ User _}</th>
            <th>{_ In use since _}</th>
            <th>{_ Type _}</th>
        </tr>
    </thead>
    <tbody>
        {% for token in tokens %}
        <tr>
            <td><a href="{{ m.rsc[token.user_id].page_url }}">{{ m.rsc[token.user_id].title }}</a></td>
            <td>{{ token.timestamp|date:"Y-d-m H:i" }}</td>
            <td>{{ token.token_type }}</td>
        </tr>
    </tbody>
</table>
{% endfor %}
{% else %}
<p>{_ This token is not yet used by anybody. _}</p>
{% endif %}

{% if delete %}
<p>{_ Do you really want to delete this application? _}</p>
<div class="modal-footer">
    {% button class="btn" action={dialog_close} text=_"Cancel" %}
    {% button class="btn btn-primary" text=_"Delete this application!" postback={confirm_del_app id=id} %}
</div>
{% else %}
<div class="modal-footer">
    {% button class="btn" action={dialog_close} text=_"Close" %}
</div>
{% endif %}

{% endwith %}
{% endwith %}
