<form id="logon" method="post" action="{% url admin_logon %}">
	<fieldset>
		<div class="form-item">
			<label for="zp-username">{_ Name _}</label>
			<input type="text" name="zp-username" id="zp-username" value="" />
		</div>
		<div class="form-item">
			<label for="zp-password">{_ Password _}</label>
			<input type="password" name="zp-password" id="zp-password" value="" />
		</div>

		<input type="hidden" name="redirect" value="{{ redirect|escape }}" />

		<div class="form-item clearfix">
			{% button type="submit" text=_"Logon" %}
			{% button text=_"Cancel" action={redirect back} %}
		</div>
	</fieldset>
</form>
