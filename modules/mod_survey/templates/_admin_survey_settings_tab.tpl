{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}{{ m.rsc.survey.title }}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey{% endblock %}

{% block widget_before %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block widget_content %}
 	<fieldset>
 		<legend>{_ Answering _}</legend>
		<div class="form-group">
			<div class="checkbox"><div class="checkbox"><label>
				{% if id.is_a.poll %}
					<input type="hidden" name="survey_show_results" id="survey_show_results" value="1" />
					<input type="checkbox" disabled="disabled" checked="checked" />
				{% else %}
					<input type="checkbox" name="survey_show_results" id="survey_show_results" value="1" {% if id.survey_show_results %}checked="checked"{% endif %} />
				{% endif %}
				{_ Show results to user after completion of survey _}
			</label></div></div>
			<div class="checkbox"><label>
				<input type="checkbox" name="survey_multiple" id="survey_multiple" value="1" {% if id.survey_multiple or id.survey_multiple|is_undefined %}checked="checked"{% endif %} />
				{_ Allow multiple entries per user/browser _}
			</label></div>
			<div class="checkbox"><label>
				<input type="checkbox" name="survey_anonymous" id="survey_anonymous" value="1" {% if id.survey_anonymous %}checked="checked"{% endif %} /> {_ Hide the user’s id or browser-id from result exports _}
			</label></div>
		</div>
	</fieldset>

	<fieldset>
		<legend>{_ Progress _}</legend>
		<div class="form-group">
			<div class="radio"><label>
				<input type="radio" name="survey_progress" id="survey_progress_none" value="" {% if not id.survey_progress %}checked="checked"{% endif %} />
				{_ Hide progress information _}
			</label></div>
			<div class="radio"><label>
				<input type="radio" name="survey_progress" id="survey_progres_nr" value="nr" {% if id.survey_progress == 'nr' %}checked="checked"{% endif %} />
				{_ Show progress information as “<em>Question 3/10</em>” _}
			</label></div>
			<div class="radio"><label>
				<input type="radio" name="survey_progress" id="survey_progress_bar" value="bar" {% if id.survey_progress == 'bar' %}checked="checked"{% endif %} />
				{_ Show progress bar _}
			</label></div>
		</div>
	</fieldset>

	<fieldset>
		<legend>{_ Handling _}</legend>

		<div class="form-group">
			<label class="control-label">{_ Mail filled in surveys to _}</label>
			<div>
				<input type="text" class="col-md-8 form-control" name="survey_email" id="survey_email" value="{{ id.survey_email }}" placeholder="email@example.com" />
			</div>
		</div>

		{% if m.survey.handlers|length %}
		<div class="form-group">
			<label class="control-label">{_ Handle this survey with _}</label>
			<div>
				<select class="form-control" name="survey_handler" id="survey_handler">
					<option value=""></option>
					{% for val,desc in m.survey.handlers %}
						<option value="{{ val }}" {% if id.survey_handler == val %}selected{% endif %}>{{ desc }}</option>
					{% endfor %}
				</select>
			</div>
		</div>
		{% endif %}
	</fieldset>
{% endblock %}
