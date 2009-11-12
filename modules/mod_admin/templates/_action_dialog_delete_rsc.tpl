{# Used by the action dialog_delete_rsc #}
<p>Are you sure you want to delete the page “{{ m.rsc[id].title }}”?</p>

<p>This can't be undone. Your page will be lost forever.</p>

{% button text="Delete" action={delete_rsc id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
