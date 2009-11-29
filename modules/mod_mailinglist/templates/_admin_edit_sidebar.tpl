{% if r.is_a.mailinglist %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">Mailing list</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item clearfix admin-form">
		<div class="notification notice">
			Mailing list settings. <a href="javascript:void(0)" class="do_dialog {title: 'Help about mailing lists.', text: '<h3>Recipients</h3><p>To add, remove or view the mailing list recipients, click on the “show all recipients” link.</p><h3>Automatic upload of recipient lists</h3><p>The dropbox filename is used for the automatic upload of complete recipients list. The filename must match the filename of the uploaded recipient list. The complete list of recipients will be replaced with the recipients in the dropbox file.</p><h3>Access control</h3><p>Everybody who can edit a mailing list is also allowed to send a page to the mailing list. Everybody who can view the mailing list is allowed to add an e-mail address to the mailing list.</p>', width: '450px'}">Need more help?</a>
		</div>
		<p><a href="{% url admin_mailinglist_recipients id=id %}">Show all recipients &raquo;</a></p>

		<fieldset>
			<div class="form-item clear">
				<label>Dropbox filename (optional)</label>
				<input type="text" name="mailinglist_dropbox_filename" value="{{ r.mailinglist_dropbox_filename }}" />
			</div>
		</fieldset>
	</div>
</div>
{% endif %}

{% with m.mailinglist.scheduled[id] as scheduled %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: {{ scheduled|yesno:"false,true" }} }">
		<span class="title">Send to mailing list</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item clearfix admin-form">
		<div class="notification notice">
			Send this page to a mailing list. <a href="javascript:void(0)" class="do_dialog {title: 'Help about mailing lists.', text: 'You can send this page as an e-mail to everyone in a mailing list. If the page has a future publication start date, is not yet published or is not visible for the world then the mail will be send after publication of the page.', width: '450px'}">Need more help?</a>
		</div>

		{% button text="Send mailing" title="Select a mailing list to send this page to." class="do_tooltip" action={dialog_mailing_page id=id} %}
		
		{% button text="Send test mailing" class="do_tooltip" disabled=m.rsc.mailinglist_test.is_visible|not title="Send this page to the test mailing list." action={mailing_page_test id=id} %}

		{% button text="Email page" title="Send this page to an e-mail address." class="do_tooltip" action={dialog_email_page id=id} %}

		<div class="clear"></div>

		<div id="mailinglist-scheduled">
			{% include "_mailinglist_scheduled.tpl" id=id scheduled=scheduled %}
		</div>
	</div>
</div>
{% endwith %}
