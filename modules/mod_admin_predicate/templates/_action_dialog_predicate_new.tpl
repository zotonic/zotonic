
<p>{_ Please fill in the title of the new predicate. _}</p>

{% wire id=#form type="submit" postback="predicate_new" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="redirect" value="{{ redirect }}" />

	<div class="new-predicate-wrapper">
		<p>
			<label for="new_predicate_title" style="color:white">{_ Title _}</label>
			<input type="text" id="new_predicate_title" name="new_predicate_title" value="{{ title|escape }}" />
			{% validate id="new_predicate_title" type={presence} %}
		</p>

		<button type="submit">{_ Make predicate _}</button>

		{% button action={dialog_close} text=_"Cancel" %}
	</div>
</form>

