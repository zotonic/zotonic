{% wire id=#survey_edit_form type="submit" delegate=delegate postback={survey_question_save id=id question_id=question_id} %}
<form id="{{ #survey_edit_form }}" action="postback">

	<h3>{_ Edit _} {{ type }}</h3>

	<p>{{ explanation|default:_"Please enter a name of your question (for reporting), and the question that will be shown to the people surveyed." }}</p>

	{% if has_name %}
		<label for="{{ #name }}">{_ Name _}</label>
		<input type="text" id="{{ #name }}" name="name" value="{{ name|escape }}" maxlength="32" />
	{% endif %}
	
	{% if has_question %}
		<label for="{{ #question }}">{{ question_label|default:_"Question" }}</label>
		<input type="text" id="{{ #question }}" name="question" value="{{ question|escape }}" />
	{% endif %}
	
	{% if has_text %}
		{{ text_explanation }}
		<label for="{{ #text }}">{{ text_label|default:_"Choices" }}</label>
		<textarea rows="10" type="text" id="{{ #text }}" name="text">{{ text|escape }}</textarea>
	{% endif %}

    {% if has_name or has_question or has_text %}
	<div class="clearfix">
		<label for="{{ #required }}">{_ Required? _}</label>
		<input type="checkbox" id="{{ #required }}" name="is_required" {% if is_required %}checked="checked"{% endif %} value="1" />
		{_ Whether the question needs answering or not. _}
	</div>

	{% button text=_"Save" %}
	{% endif %}

	{% button text=_"Cancel" action={dialog_close} %}

</form>
