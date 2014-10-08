{% extends "admin_edit_widget_std.tpl" %}

{# Make the widget conditional, based on the config value #}
{% block widget_wrapper %}
    {% if m.config.mod_backup.admin_panel.value %}
    {% inherit %}
    {% endif %}
{% endblock %}

{% block widget_title %}{_ Backup &amp; Restore _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}admin_backup_sidebar{% endblock %}

{% block widget_content %}
<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-xs do_dialog" data-dialog="title: '{{ _"Help about backup &amp; restore."|escapejs }}', text: '{{ _"Click on <b>List and restore an earlier version</b> to get an overview of earlier saved versions of this page.<br/>You can also save the complete contents of a page to a file. Later you can reload this file, replacing the current page contents. Note that the file does not contain your unsaved changes. Connections and media are not saved as well."|escapejs }}'" title="{_ Need more help? _}"><i class="glyphicon glyphicon-question-sign"></i></a>
</div>


<div class="form-group">
    <p>
    	<a href="{% url admin_backup_revision id=id %}">{_ List and restore an earlier version _}</a>
    </p>

    <div>
    	<a href="{% url rest_rsc id=id format="bert" %}" class="btn btn-default">{_ Download backup file _}</a>
    	{% button text=_"Restore backup" class="btn btn-default" 
    		action={dialog_open title=_"Restore backup" template="_dialog_backup_upload.tpl" id=id} %}
    </div>
</div>

{% endblock %}
