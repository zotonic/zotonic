
<p>{_ Type the title of the page you want to connect to. _}</p>

<div class="form-item autocomplete-wrapper clear">
	<input id="{{#input}}" class="autocompleter" type="text" value="" />
	<ul id="{{#suggestions}}" class="suggestions-list"></ul>
</div>

{% wire id=#input
	type="keyup" 
	action={typeselect
				target=#suggestions 
				action_with_id={with_args action={zlink} arg={id select_id}}
				action={dialog_close}
			}
%}
