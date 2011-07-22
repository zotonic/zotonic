{# Used by the action dialog_config_delete #}
<p>{_ Are you sure you want to delete the configuration key _} “{{ module|escape }}.{{ key|escape }}”?</p>

<p>{_ This can't be undone. The configuration key will be lost forever. _}</p>

{% button text=_"Delete" action={config_delete module=module key=key on_success=on_success} action={dialog_close} %}
{% button text=_"Cancel" action={dialog_close} %}
