{% lib "js/z.admin-edit-version.js" %}
{% lib "css/admin-edit-version.css" %}
<input type="hidden" name="z_edit_version" value="{{ edit_version|default:id.version }}">
<div class="alert alert-warning{% if not is_dialog %} admin-edit-version-warning-fixed{% endif %}" role="alert" data-edit-version-warning hidden>
    <button type="button" class="close" data-edit-version-dismiss aria-label="{_ Close _}" title="{_ Close _}">
        <span aria-hidden="true">&times;</span>
    </button>
    <span>{% if is_overview %}{_ This page has changed. Reload to see the latest translations. _}{% else %}{_ This page has changed elsewhere. Reload to get the latest version before saving. _}{% endif %}</span>
    <button type="button" class="btn btn-warning" data-edit-version-reload
            data-confirm="{_ Reloading discards any unsaved changes in this form. Continue? _}">{_ Reload page _}</button>
</div>
{% javascript %}
    window.z_admin_edit_version.watch("{{ version_form|default:"rscform"|escapejs }}", {{ id }});
{% endjavascript %}
