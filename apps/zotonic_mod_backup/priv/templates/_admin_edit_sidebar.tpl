{% extends "admin_edit_widget_std.tpl" %}

{# Make the widget conditional, based on the config value #}
{% block widget_wrapper %}
    {% if m.backup.admin_panel %}
        {% inherit %}
    {% endif %}
{% endblock %}

{% block widget_title %}
{_ Backup &amp; Restore _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
        %{
            title: _"Help about backup &amp; restore",
            text: _"Click on <b>List and restore an earlier version</b> to get an overview of earlier saved versions of this page.<br/>You can also save the complete contents of a page to a file. Later you can reload this file, replacing the current page contents. Note that the file does not contain your unsaved changes. Connections and media are not saved as well."
        }|escape
    }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}admin_backup_sidebar{% endblock %}

{% block widget_content %}
<div class="form-group">
    {% if id.is_editable %}
        <p>
        	<a href="{% url admin_backup_revision id=id %}">{_ List and restore an earlier version _}</a>
        </p>
    {% endif %}

    <p>
        {% if m.modules.active.mod_export %}
            <a href="{% url export_rsc type="bert" id=id %}" class="btn btn-default">{_ Download backup file _}</a>
        {% endif %}
    	{% button
    	    text=_"Restore backup"
    	    class="btn btn-default"
    		action={dialog_open
    		    title=_"Restore backup"
    		    template="_dialog_backup_upload.tpl"
    		    id=id
    		}
    	%}
    </p>

    {% if not m.modules.active.mod_export %}
        <p class="text-warning"><i class="fa fa-info-circle"></i> {_ Enable <tt>mod_export</tt> to download backup files. _}</p>
    {% endif %}
</div>

{% endblock %}
