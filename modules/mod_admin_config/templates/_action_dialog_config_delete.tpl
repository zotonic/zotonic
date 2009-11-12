{# Used by the action dialog_config_delete #}
<p>Are you sure you want to delete the configuration key “{{ module|escape }}.{{ key|escape }}”?</p>

<p>This can't be undone. The configuration key will be lost forever.</p>

{% button text="Delete" action={config_delete module=module key=key on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
