{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}
{_ Settings _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey{% endblock %}

{% block widget_before %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block widget_content %}
 	<fieldset>
 		<legend style="margin-bottom: 0">{_ Answering _}</legend>
		<div class="form-group">
            <div class="checkbox">
	        	<label>
		            <input type="checkbox" name="survey_is_autostart" id="survey_is_autostart" value="1" {% if id.survey_is_autostart or (id.survey_is_autostart|is_undefined and id.is_a.poll) %}checked="checked"{% endif %} />
		            {_ Immediately start with the questions, no “Start” button _}
	            </label>
	        </div>
			<div class="checkbox">
				<label>
					<input type="checkbox" name="survey_anonymous" id="survey_anonymous" value="1" {% if id.survey_anonymous %}checked="checked"{% endif %} /> {_ Hide the user’s id or browser-id from result exports _}
				</label>
			</div>
		</div>

		<div class="form-group">
        	<label class="control-label">{_ Fill in _}</label>
        	<div class="controls">
				<select class="form-control" name="survey_multiple" id="survey_multiple">
					<option value="0">{_ Once only per user/browser _}</option>
					<option value="1" {% if id.survey_multiple == 1 %}selected{% endif %}>
						{_ Multiple entries per user/browser _}
					</option>
					<option value="2" {% if id.survey_multiple == 2 %}selected{% endif %}>
						{_ Once and allow editing afterwards _}
					</option>
				</select>
			</div>
		</div>

		{% if not id.is_a.poll %}
			<div class="form-group">
				<label class="control-label">{_ When finished show _}</label>
				<div class="controls">
					<select class="form-control" name="survey_show_results" id="survey_show_results">
						<option value="">{_ Thank you text only _}</option>
						<option value="1" {% if id.survey_show_results == 1 %}selected{% endif %}>
							{_ Aggregated results from all respondents (only results for multiple choice questions are shown) _}
						</option>
						<option value="2" {% if id.survey_show_results == 2 %}selected{% endif %}>
							{_ Results from the respondent _}
						</option>
					</select>
				</div>
			</div>
			<div class="control-group">
			    <label class="control-label">{_ Test pass percentage _}</label>
			    <div class="controls">
			        <input type="text" name="survey_test_percentage" id="{{ #survey_test_percentage }}" 
			               class="input-small" value="{{ id.survey_test_percentage }}" placeholder="" /> %
			        {% validate id=#survey_test_percentage name="survey_test_percentage"
			        			type={numericality minimum=0 maximum=100}
			        %}
			        <p class="help-block muted">{_ Leave empty if there are no test questions. _}</p>
			    </div>
			</div>
		{% else %}
			<input type="hidden" name="survey_show_results" id="survey_show_results" value="1" />
		{% endif %}
	</fieldset>

	<fieldset>
		<legend style="margin-bottom: 0">{_ Progress _}</legend>
		<div class="form-group">
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progress_none" value="" {% if not id.survey_progress %}checked="checked"{% endif %} />
					{_ Hide progress information _}
				</label>
			</div>
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progres_nr" value="nr" {% if id.survey_progress == 'nr' %}checked="checked"{% endif %} />
					{_ Show progress information as “<em>Question 3/10</em>” _}
				</label>
			</div>
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progress_bar" value="bar" {% if id.survey_progress == 'bar' %}checked="checked"{% endif %} />
					{_ Show progress bar _}
				</label>
			</div>
		</div>
	</fieldset>

	<fieldset>
		<legend style="margin-bottom: 0">{_ Handling _}</legend>

		<div class="form-group">
			<label class="control-label">{_ Mail results to _}</label>
			<div>
				<input type="text" class="col-md-8 form-control" name="survey_email" id="survey_email" value="{{ id.survey_email }}" placeholder="email@example.com" />
				<p class="help-block">
					{_ Separate multiple email addresses with a comma. _}
				</p>
			</div>
		</div>

		<div class="form-group">
			<div class="checkbox">
				<label>
					<input type="checkbox" name="survey_email_respondent" id="survey_email_respondent" value="1" {% if id.survey_email_respondent  %}checked{% endif %} />
					{_ Send confirmation with the results to the email address entered in this form (you must include a question labeled “email”). _}
				</label>
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
