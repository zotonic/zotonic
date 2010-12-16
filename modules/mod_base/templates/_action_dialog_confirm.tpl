<div class="confirm">
	<p>{{ text }}</p>

	<div class="buttons">
		<button id="{{ #ok }}">{{ ok|default:_"OK" }}</button>
		<button id="{{ #cancel }}" class="cancel shy">{{ cancel|default:_"Cancel" }}</button>
		{% wire id=#ok 
				action={dialog_close}
				action=action
				postback=postback
				delegate=delegate
		%}
		{% wire id=#cancel 
				action={dialog_close}
				action=on_cancel
		%}
	</div>
</div>
