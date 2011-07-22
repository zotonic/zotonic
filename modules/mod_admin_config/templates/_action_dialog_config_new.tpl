<p>{_ Please fill in the module, key and value for the new configuration key. _}</p>

{% wire id=#form type="submit" postback={config_new on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">
	<div class="new-predicate-wrapper">
		<div class="form-item clearfix">
			<label for="{{ #module }}">{_ Module _}</label>
			<input type="text" id="{{ #module }}" name="module" value="" />
			{% validate id=#module name="module" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<label for="{{ #key }}">{_ Key _}</label>
			<input type="text" id="{{ #key }}" name="key" value="" />
			{% validate id=#key name="key" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<label for="{{ #value }}">{_ Value _}</label>
			<input type="text" id="{{ #value }}" name="val" value="" />
		</div>

		<button type="submit">{_ Add key _}</button>
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>

