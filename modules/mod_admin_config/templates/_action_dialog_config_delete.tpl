{# Used by the action dialog_config_delete #}
<p>{_ Are you sure you want to delete the configuration key _} “{{ module|escape }}.{{ key|escape }}”?</p>

<p>{_ This can't be undone. The configuration key will be lost forever. _}</p>

<div class="modal-footer">
    {% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button class="btn btn-primary" text=_"Delete" action={config_delete module=module key=key on_success=on_success} action={dialog_close} %}
</div>
