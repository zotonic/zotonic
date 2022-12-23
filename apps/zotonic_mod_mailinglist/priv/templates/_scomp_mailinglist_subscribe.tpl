{% with m.rsc[id].id as id %}
{% if id.is_visible %}
<div id="mailinglist_subscribe-{{ id }}" class="mailinglist_subscribe">
	{% if is_email_only %}
		<p>{_ Subscribe to _} {{ id.title }}.</p>
	{% else %}
		<p>
			{_ Give your e-mail address to subscribe to _} {{ id.title }}.
			{_ You will receive a confirmation in your e-mail._}
		</p>
	{% endif %}

	<div id="mailinglist_subscribe_form-{{ id }}" class="mailinglist_subscribe__form">
        {% if in_admin %}
			{% include "_mailinglist_subscribe_form.tpl" id=id recipient_id=recipient_id make_person=make_person %}
        {% else %}
			{% include "_mailinglist_subscribe_form.tpl" id=id recipient_id=recipient_id make_person=make_person %}
        {% endif %}
	</div>

	<div id="mailinglist_subscribe_done-{{ id }}" style="display:none" class="mailinglist_subscribe__done">
		<h2>{_ Thank you _}</h2>
	 	<p>
		{_ Your e-mail address is added to the mailing list. A confirmation mail is sent to your e-mail address and will arrive shortly. When you don’t receive it, then please check your spam folder. _}
		</p>
	</div>

	<p id="mailinglist_subscribe_error-{{ id }}" style="display:none" class="mailinglist_subscribe__error error">
		{_ Sorry, I could not subscribe you to the mailing list. Please try again later or with another e-mail address. _}
	</p>
</div>
{% endif %}
{% endwith %}
