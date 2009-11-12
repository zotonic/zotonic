{# Used by the action dialog_predicate_delete #}
<p>Are you sure you want to delete the predicate “{{ m.rsc[id].title }}”?</p>

<p>This can't be undone. The predicate will be lost forever.</p>

{% button text="Delete" action={predicate_delete id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
