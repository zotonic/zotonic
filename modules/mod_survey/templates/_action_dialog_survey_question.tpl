{% wire id=#survey_edit_form type="submit" delegate=delegate postback={survey_question_save id=id question_id=question_id} %}
<form id="{{ #survey_edit_form }}" action="postback" class="form-horizontal">

    <p>{{ explanation|default:_"Please enter a name of your question (for reporting), and the question that will be shown to the people surveyed." }}</p>

    {% if has_name %}
    <div class="control-group">
        <label class="control-label" for="{{ #name }}">{_ Name _}</label>
        <div class="controls">
	    <input type="text" id="{{ #name }}" name="name" value="{{ name|escape }}" maxlength="32" class="do_autofocus" />
        </div>
    </div>
    {% endif %}
	
    {% if has_question %}
    <div class="control-group">
        <label class="control-label" for="{{ #question }}">{{ question_label|default:_"Question" }}</label>
        <div class="controls">
	    <input type="text" id="{{ #question }}" name="question" value="{{ question|escape }}" />
        </div>
    </div>
    {% endif %}
	
    {% if has_text %}
    {{ text_explanation }}
    <div class="control-group">
	<label class="control-label" for="{{ #text }}">{{ text_label|default:_"Choices" }}</label>
        <div class="controls">
	    <textarea rows="10" type="text" id="{{ #text }}" name="text">{{ text|escape }}</textarea>
        </div>
    </div>
    {% endif %}

    {% if has_name or has_question or has_text %}
    <div class="control-group">
	<label class="control-label" for="{{ #required }}">{_ Required? _}</label>
        <div class="controls">
	    <label class="checkbox inline"><input type="checkbox" id="{{ #required }}" name="is_required" {% if is_required %}checked="checked"{% endif %} value="1" />
	    {_ Whether the question needs answering or not. _}
            </label>
        </div>
    </div>
    {% endif %}

    <div class="modal-footer">
	{% button class="btn" text=_"Cancel" action={dialog_close} %}
        {% button class="btn btn-primary" text=_"Save" %}
    </div>
    
</form>
