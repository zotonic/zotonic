<div id="mailinglist_subscribe">
	{% if is_email_only %}
		<p>{_ Subscribe to _} {{ m.rsc[id].title }}.</p>
	{% else %}
		<p>
			{_ Give your e-mail address to subscribe to _} {{ m.rsc[id].title }}.  
			{_ You will receive a confirmation in your e-mail._}
		</p>
	{% endif %}

	<div id="mailinglist_subscribe_form" class="clearfix">
        {% if in_admin %}
		{% include "_mailinglist_subscribe_form.tpl" id=id recipient_id=recipient_id make_person=make_person %}
        {% else %}
        {% with m.rsc[user_id] as r %}
		{% include "_mailinglist_subscribe_form.tpl" id=id recipient_id=recipient_id make_person=make_person r=r %}
        {% endwith %}
        {% endif %}
	</div>

	<div id="mailinglist_subscribe_done" style="display:none">
		<h2>{_ Thank you _}</h2>
	 	<p>
		{_ Your e-mail address is added to the mailing list. A confirmation mail is sent to your e-mail address and will arrive shortly. When you don’t receive it, then please check your spam folder. _}
		</p>
	</div>
	
	<p id="mailinglist_subscribe_error" style="display:none" class="error">
		{_ Sorry, I could not subscribe you to the mailing list. Please try again later or with another e-mail address. _}
	</p>
</div>
