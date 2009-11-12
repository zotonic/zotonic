{# Used by the action dialog_group_delete #}
<p>Are you sure you want to delete the group “{{ m.rsc[id].title }}”?</p>

<p>This can't be undone. The group will be lost forever.</p>

{% button text="Delete" action={group_delete id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
