{# Used by the action dialog_delete_rsc #}
<p>{_ Are you sure you want to delete the page _} “{{ m.rsc[id].title }}”?</p>

<p>{_ This can't be undone. Your page will be lost forever. _}</p>

{% button text=_"Delete" action={delete_rsc id=id on_success=on_success} action={dialog_close} %}
{% button text=_"Cancel" action={dialog_close} %}
