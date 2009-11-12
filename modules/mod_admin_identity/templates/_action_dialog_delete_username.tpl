{# Used by the action dialog_delete_username #}
<p>Are you sure you want to delete the username from “{{ m.rsc[id].title }}”?</p>

<p>This can't be undone. The username and password will be lost forever.</p>

{% button text="Delete" action={delete_username id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
