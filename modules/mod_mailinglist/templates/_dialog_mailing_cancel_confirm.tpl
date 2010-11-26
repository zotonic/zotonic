<p>{_ Are you sure you want to cancel the mailing to _} “{{ m.rsc[mailinglist_id].title }}”?</p>
	
{% button text=_"Cancel mailing" postback={mailing_cancel mailinglist_id=mailinglist_id page_id=page_id} action={dialog_close} %}
{% button text=_"Keep mailing" action={dialog_close} %}