
<p>Please fill in the title of the new group.</p>

{% wire id=#form type="submit" postback="group_new" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="redirect" value="{{ redirect }}" />

	<div class="new-predicate-wrapper">
		<p>
			<label for="new_group_title" style="color:white">Title</label>
			<input type="text" id="new_group_title" name="new_group_title" value="{{ title|escape }}" />
			{% validate id="new_group_title" type={presence} %}
		</p>
		
		<button type="submit">Make group</button>
		
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>

