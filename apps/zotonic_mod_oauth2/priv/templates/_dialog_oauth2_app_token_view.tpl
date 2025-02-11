{% if m.oauth2.apps[app_id] as app %}
    <p>
        {_ Here is the new access token. _}</p>
    </p>

    <p>
        <b>{_ Be careful, this access token gives full access to your account. _}</b>
    </p>


    <div class="alert alert-info" style="text-align: center">
        <p><tt>{{ token|escape }}</tt></p>
        <p>
            <a class="btn btn-xs btn-default"
               data-onclick-topic="model/clipboard/post/copy"
               data-text="{{ token|escape }}"
            >
                <span class="fa fa-copy"></span> {_ Copy _}
            </a>
        </p>
    </div>

    <p class="text-muted">
        {_ You will not be able to see this token again, so copy it and save it in a secure place. _}
    </p>
{% else %}
    <p class="alert alert-danger">
        {_ App not found, or no view permission. _}
    </p>
{% endif %}

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action=action|default:{dialog_close} %}
</div>
