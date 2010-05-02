<div id="mailinglist_subscribe">
	<p>
		Give your e-mail address to subscribe to {{ m.rsc[id].title }}.  
		You will receive a confirmation in your e-mail.
	</p>

	<div id="mailinglist_subscribe_form" class="clearfix">
		{% include "_mailinglist_subscribe_form.tpl" id=id make_person=make_person %}
	</div>

	<div id="mailinglist_subscribe_done" style="display:none">
		<h2>Thank you</h2>
	 	<p>
		Your e-mail address is added to the mailing list. 
		A confirmation mail is sent to your e-mail address and will arrive shortly.
		When you don’t receive it, then please check your spam folder.
		</p>
	</div>
	
	<p id="mailinglist_subscribe_error" style="display:none" class="error">
		Sorry, I could not subscribe you to the mailing list. Please try again later or with another e-mail address.
	</p>
</div>
