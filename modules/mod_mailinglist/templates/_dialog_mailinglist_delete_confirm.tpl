<p>
	{_ Are you sure you want to delete the mailing list _} “{{ m.rsc[id].title }}”? <br/>
	{_ The recipients list of the mailing list will be deleted as well. _}
</p>

<p>{_ This can not be undone _}</p>

{% button text=_"delete" postback={mailinglist_delete id=id} %}
{% button text=_"cancel" action={dialog_close} %}
