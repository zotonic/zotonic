{% with m.mailinglist.scheduled[id] as scheduled %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: {{ scheduled|yesno:"false,true" }} }">
		<span class="title">{_ Send to mailing list _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	<div class="item clearfix admin-form">
		<div class="notification notice">
			{_ Send this page to a mailing list. _} <a href="javascript:void(0)" class="do_dialog {title: '{_ Help about mailing lists. _}', text: '{_ You can send this page as an e-mail to everyone in a mailing list. If the page has a future publication start date, is not yet published or is not visible for the world then the mail will be send after publication of the page. _}', width: '450px'}">{_ Need more help? _}</a>
		</div>

		{% button text=_"Send mailing" title=_"Select a mailing list to send this page to." class="do_tooltip" action={dialog_mailing_page id=id} %}
		
		{% button text=_"Send test mailing" class="do_tooltip" disabled=not m.rsc.mailinglist_test.is_visible title=_"Send this page to the test mailing list." action={mailing_page_test id=id} %}

		{% button text=_"Email page" title=_"Send this page to an e-mail address." class="do_tooltip" action={dialog_email_page id=id} %}

		<div class="clear"></div>

		<div id="mailinglist-scheduled">
			{% include "_mailinglist_scheduled.tpl" id=id scheduled=scheduled %}
		</div>
	</div>
</div>
{% endwith %}
