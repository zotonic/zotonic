{# Used by the action dialog_predicate_delete #}

<p>{_ Are you sure you want to delete the predicate _} “{{ m.rsc[id].title }}”?</p>
<p>{_ This can't be undone. The predicate will be lost forever. _}</p>

<div class="modal-footer">
    {% button class="btn" text="Cancel" action={dialog_close} %}
    {% button class="btn btn-primary" text="Delete" action={predicate_delete id=id on_success=on_success} action={dialog_close} %}
</div>
