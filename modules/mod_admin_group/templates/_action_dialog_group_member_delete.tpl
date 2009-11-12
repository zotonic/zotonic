{# Used by the action dialog_group_member_delete #}
<p>Are you sure you want to remove “{{ m.rsc[member_id].title }}” from the group “{{ m.rsc[id].title }}”?</p>

<p>This can't be undone.</p>

{% button text="Delete" action={group_member_delete id=id member_id=member_id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
