<div class="modal-body">
    <p>{% trans "Review the latest version for {name}." name=name|escape %}</p>

    {% if is_module %}
        <p class="text-muted">{_ Deployment updates the entire Git repository, including any other modules it contains. _}</p>
    {% endif %}

    {% if preview.current %}
        <h4>{_ Current commit _}</h4>
        <div class="list-group">
            <div class="list-group-item">
                {% include "_site_deploy_commit.tpl" commit=preview.current %}
            </div>
        </div>
    {% endif %}

    {% if preview.error %}
        <p class="alert alert-danger" role="alert">
            {% if preview.error == `eacces` %}
                {_ You are not allowed to deploy this code. _}
            {% elseif preview.error == `no_upstream` %}
                {_ This branch has no upstream configured. Configure its remote tracking branch before deploying. _}
            {% elseif preview.error == `no_vcs` %}
                {_ No deployable version control repository was found. _}
            {% elseif preview.error == `fetch_failed` %}
                {_ Could not fetch the remote change log. Check the remote connection and try again. _}
            {% else %}
                {_ The deployment preview is unavailable. Check the repository and server log, then try again. _}
            {% endif %}
        </p>
    {% else %}
        <h4 id="{{ #changes_heading }}">{_ Incoming changes _} {% if preview.count %}<span class="badge">{{ preview.count|escape }}</span>{% endif %}</h4>
        {% if preview.upstream %}
            <p class="text-muted">{{ preview.upstream|escape }}</p>
        {% endif %}
        {% if preview.log %}
            <div class="pre-scrollable" tabindex="0" role="region" aria-labelledby="{{ #changes_heading }}">
                <ol class="list-group list-unstyled">
                    {% for commit in preview.log %}
                        <li class="list-group-item">
                            {% include "_site_deploy_commit.tpl" commit=commit %}
                        </li>
                    {% endfor %}
                </ol>
            </div>
            {% if preview.is_truncated %}
                <p class="text-muted">{_ Showing the most recent 100 incoming commits. _}</p>
            {% endif %}
        {% else %}
            <p>{_ There are no incoming changes. _}</p>
        {% endif %}
        {% if preview.is_diverged %}
            <p class="alert alert-warning" role="alert">{_ The local branch cannot be fast-forwarded to the remote version. Resolve the local commits before deploying. _}</p>
        {% endif %}
    {% endif %}
</div>
<div class="modal-footer">
    {% include "_site_deploy_status.tpl" %}
    {% button class="btn btn-default" text=_"Reset" postback={vcs_reset target=target} delegate=`mod_site_update` %}
    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
    {% if preview.can_deploy %}
        {% button class="btn btn-primary" text=_"Deploy" postback={vcs_deploy target=target} delegate=`mod_site_update` %}
    {% else %}
        <button type="button" class="btn btn-primary" disabled>{_ Deploy _}</button>
    {% endif %}
</div>
