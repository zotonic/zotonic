<div class="modal-body">
    <p>{% trans "Local changes for {name}." name=name|escape %}</p>
    {% if preview.error %}
        <p class="alert alert-danger" role="alert">
            {% if preview.error == `update_in_progress` %}
                {_ A deployment is running. Try again when it finishes. _}
            {% else %}
                {_ Could not list local changes. Check the repository and server log. _}
            {% endif %}
        </p>
    {% else %}
        {% if preview.changes %}
            <pre class="pre-scrollable" tabindex="0">{{ preview.changes|escape }}</pre>
        {% else %}
            <p>{_ There are no local changes. You can still rebuild to recover from a failed deployment. _}</p>
        {% endif %}
        <p class="alert alert-warning">
            {_ This permanently discards all uncommitted changes in the entire repository, including staged changes, and deletes untracked files and directories. Ignored files, local commits and nested repositories are preserved. _}
        </p>
        <p>{_ The code is then rebuilt before deployment is enabled again. If this repository contains other sites or modules, their local changes are also discarded. _}</p>
    {% endif %}
</div>
<div class="modal-footer">
    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
    {% if not preview.error %}
        {% button class="btn btn-danger" text=_"Undo all local changes and rebuild"
            postback={vcs_reset_confirm target=target} delegate=`mod_site_update` %}
    {% endif %}
</div>
