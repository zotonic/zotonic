{# Used by the action dialog_delete_rsc #}
<p>{_ Are you sure you want to delete the page _} “{{ m.rsc[id].title }}”?</p>
<p>{_ This can't be undone. Your page will be lost forever. _}</p>

<div class="modal-footer">
    {% button class="btn" text=_"Cancel" action={dialog_close} %}
    {% button class="btn btn-primary" text=_"Delete" action={delete_rsc id=id on_success=on_success} action={dialog_close} %}
</div>
