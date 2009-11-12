
<p>Select a role and type the name to find the new group member.</p>

{% wire id=#form type="submit" postback="group_member_add" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />

	<div class="new-predicate-wrapper">

		<div class="form-item clearfix">
			<input id="{{ #leader }}" type="radio" class="xdo_fieldreplace" name="member" value="leader"  />
			<label for="{{ #leader }}">Leader</label>
		</div>

		<div class="form-item clearfix">
			<input id="{{ #observer }}" type="radio" class="xdo_fieldreplace" name="member" value="observer"  />
			<label for="{{ #observer }}">Observer</label>
		</div>

		<div class="form-item clearfix">
			<input id="{{ #member }}" type="radio" class="xdo_fieldreplace" name="member" value="member"  checked="checked" />
			<label for="{{ #member }}">Member</label>
		</div>

		<p>Type text to search on and click on the member you want to add:</p>
		<div class="form-item autocomplete-wrapper clear">
			<input id="{{#input}}" class="autocompleter" type="text" value="" />
			<ul id="{{#suggestions}}" class="suggestions-list"></ul>

			{% button text="Cancel" action={dialog_close} style="float:right;" %}

		</div>

		{% wire id=#input
			type="keyup" 
			action={typeselect
						target=#suggestions
						template="_action_typeselect_result_submit.tpl"
					}
		%}
		
	</div>
</form>

