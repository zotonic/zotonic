{# Used by the action dialog_predicate_delete #}
<p>{_ Are you sure you want to delete the predicate _} “{{ m.rsc[id].title }}”?</p>

<p>{_ This can't be undone. The predicate will be lost forever. _}</p>

{% button text=_"Delete" action={predicate_delete id=id on_success=on_success} action={dialog_close} %}
{% button text=_"Cancel" action={dialog_close} %}
