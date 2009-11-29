<p>Are you sure you want to cancel the mailing to “{{ m.rsc[mailinglist_id].title }}”?</p>
	
{% button text="Cancel mailing" postback={mailing_cancel mailinglist_id=mailinglist_id page_id=page_id} action={dialog_close} %}
{% button text="Keep mailing" action={dialog_close} %}