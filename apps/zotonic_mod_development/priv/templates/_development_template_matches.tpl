<p>{_ Template option _}: <code>{{ template_option|pprint }}</code></p>
{% if is_category %}
    <p>{_ Includes the base template, category variants and resource-name variants. The selected template depends on the resource being rendered. _}</p>
{% endif %}
<p>{_ Matches with the same name are listed in module priority order. _}</p>
<div class="table-responsive">
    <table class="table table-striped">
        <thead><tr><th>{_ Template _}</th><th>{_ Module _}</th><th>{_ Actions _}</th></tr></thead>
        <tbody>
        {% for tpl in templates %}
            <tr>
                <td><strong>{{ tpl.name|escape }}</strong><br><small>{{ tpl.path|development_filename|escape }}</small></td>
                <td>{{ tpl.module|escape }}</td>
                <td>
                    {% button text=_"View" class="btn btn-primary btn-xs"
                        action={dialog_close} postback={template_view template=tpl.path} delegate=`mod_development` %}
                    {% include "_development_editor_button.tpl" editor_file=tpl.path editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
                </td>
            </tr>
        {% empty %}
            <tr><td colspan="3"><em>{_ No matching templates found. _}</em></td></tr>
        {% endfor %}
        </tbody>
    </table>
</div>
<div class="modal-footer">
    {% button text=_"Close" class="btn btn-default" action={dialog_close} %}
</div>
