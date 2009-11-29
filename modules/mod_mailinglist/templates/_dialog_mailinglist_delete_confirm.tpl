<p>
	Are you sure you want to delete the mailing list “{{ m.rsc[id].title }}”? <br/>
	The recipients list of the mailing list will be deleted as well.
</p>

<p>This can not be undone</p>

{% button text="delete" postback={mailinglist_delete id=id} %}
{% button text="cancel" action={dialog_close} %}
