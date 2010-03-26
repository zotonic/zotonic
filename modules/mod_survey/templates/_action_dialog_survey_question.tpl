{% wire id=#survey_edit_form type="submit" delegate=delegate postback={survey_question_save id=id question_id=question_id} %}
<form id="{{ #survey_edit_form }}" action="postback">

	<h3>Edit {{ type }}</h3>

	<p>{{ explanation|default:"Please enter a name of your question (for reporting), and the question that will be shown to the people surveyed." }}</p>

	{% if has_name %}
		<label for="{{ #name }}">Name</label>
		<input type="text" id="{{ #name }}" name="name" value="{{ name|escape }}" />
	{% endif %}
	
	{% if has_question %}
		<label for="{{ #question }}">{{ question_label|default:"Question" }}</label>
		<input type="text" id="{{ #question }}" name="question" value="{{ question|escape }}" />
	{% endif %}
	
	{% if has_text %}
		{{ text_explanation }}
		<label for="{{ #text }}">{{ text_label|default:"Choices" }}</label>
		<textarea rows="10" type="text" id="{{ #text }}" name="text">{{ text|escape }}</textarea>
	{% endif %}

	{% button text="Save" %}
	{% button text="Cancel" action={dialog_close} %}

</form>
