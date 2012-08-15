<p>
	{_ Are you sure you want to delete the mailing list _} “{{ m.rsc[id].title }}”? <br/>
	{_ The recipients list of the mailing list will be deleted as well. _}
</p>

<p>{_ This can not be undone _}</p>

<div class="modal-footer">
    {% button class="btn" text=_"delete" postback={mailinglist_delete id=id} tag="a" %}
    {% button class="btn btn-primary" text=_"cancel" action={dialog_close} %}
</div>
