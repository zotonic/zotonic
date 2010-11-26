{% with m.mailinglist.recipient[recipient_id] as rcpt %}

{% if recipient_id %}
{% wire id=#form type="submit" postback={recipient_edit id=id in_admin=in_admin recipient_id=recipient_id} delegate=delegate %}
{% else %}
{% wire id=#form type="submit" postback={recipient_add id=id in_admin=in_admin} delegate=delegate %}
{% endif %}

<form id="{{ #form }}" method="post" action="postback">

<div class="form-item clearfix">
	<div class="zp-60">
		<label for="{{ #email }}">{_ e-Mail _}</label>
		<input type="text" id="{{ #email }}" name="email" value="{{ rcpt.email|default:r.email|escape }}" />
		{% validate id=#email name="email" type={presence} type={email} %}
	</div>
</div>

<div class="form-item clearfix">
	<div class="zp-30">
		<div class="form-item clearfix">
			<label for="{{ #name_first }}">{_ First name _}</label>
			<input id="{{ #name_first }}" type="text" name="name_first" value="{{ rcpt.props.name_first|default:r.name_first }}" style="width: 90%;" />
		</div>
	</div>

	<div class="zp-15">
		<div class="form-item clearfix">
			<label for="{{ #name_surname_prefix }}">{_ Prefix _}</label>
			<input id="{{ #name_surname_prefix }}" type="text" name="name_surname_prefix" value="{{ rcpt.props.name_surname_prefix|default:r.name_surname_prefix }}" style="width: 50%" />
		</div>
	</div>

	<div class="zp-55">
		<div class="form-item clearfix">
			<label for="{{ #name_surname }}">{_ Surname _}</label>
			<input id="{{ #name_surname }}" type="text" name="name_surname" value="{{ rcpt.props.name_surname|default:r.name_surname }}" style="width: 90%" />
		</div>
	</div>
	
	{% if not in_admin %}
		{% validate id=#name_first name="name_first" type={presence} %}
		{% validate id=#name_surname name="name_surname" type={presence} %}
	{% endif %}

</div>

{% if in_admin and not recipient_id %}
	<div class="form-item clearfix">
		<label for="{{ #welcome }}">{_ Send welcome _}</label>
		<input type="checkbox" id="{{ #welcome }}" name="send_welcome" value="1" checked="checked" />
	</div>
{% endif %}

<div style="clear: both">
    {% if recipient_id %}
	{% button text=_"edit" %}
    {% else %}
	{% button text=_"subscribe" %}
	{% endif %}

	{% if in_admin %}
		{% button text=_"cancel" action={dialog_close} %}
	{% endif %}
</div>

</form>
{% endwith %}

