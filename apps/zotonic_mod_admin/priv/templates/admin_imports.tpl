{% extends "admin_base.tpl" %}
{% block title %}{_ Imports _}{% endblock %}
{% block content %}
<div class="admin-header">
    <h2>{_ Imports _}</h2>
    <p>{_ Upload files for background processing or use a module's import tool. _}</p>
</div>
{% if m.acl.is_admin or m.acl.use.mod_mailinglist %}
    {% compose "_admin_maintenance_panel.tpl" title=_"Upload to drop folder" %}
        {% block tools %}
            <p>{_ Files are handled in the background by mailinglist recipient import and other modules watching the drop folder. _}</p>
            {% wire id="dropupload" type="submit" postback={dropbox_upload} delegate=`mod_admin` %}
            <form id="dropupload" class="form form-inline" action="postback">
                <label for="dropupload-file" class="sr-only">{_ File to import _}</label>
                <input id="dropupload-file" class="form-control" type="file" name="file" required>
                <button class="btn btn-primary" type="submit">{_ Upload _}</button>
            </form>
        {% endblock %}
    {% endcompose %}
{% endif %}
{% all include "_admin_import_tools.tpl" %}
{% endblock %}
