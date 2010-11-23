<div class="alert">
	<p>{{ text }}</p>

	<div class="buttons">
		<button id="{{ #cancel }}">{{ button|default:_"OK" }}</button>
		{% wire id=#cancel 
				action={dialog_close}
				action=action
		%}
	</div>
</div>
