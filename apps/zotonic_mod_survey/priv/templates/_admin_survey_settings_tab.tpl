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

		<div class="row">
			<div class="form-group col-sm-6">
	        	<label class="control-label">{_ Fill in _}</label>
	        	<div class="controls">
	        		<label class="radio">
	        			<input type="radio" name="survey_multiple" value="0" {% if not id.survey_multiple %}checked{% endif %}>
	        			{_ Once only per user/browser _}
	        		</label>
	        		<label class="radio">
	        			<input type="radio" name="survey_multiple" value="1" {% if id.survey_multiple == 1 %}checked{% endif %}>
	        			{_ Multiple times per user/browser (each time with new results) _}
	        		</label>
	        		<label class="radio">
	        			<input type="radio" name="survey_multiple" value="2" {% if id.survey_multiple == 2 %}checked{% endif %}>
	        			{_ Once and allow editing afterwards _}
	        		</label>
				</div>
			</div>

			{% if not id.is_a.poll %}
				<div class="col-sm-6">
					<div class="form-group">
						<label class="control-label">{_ When finished show _}</label>
						<div class="controls">
							<label class="radio">
			        			<input type="radio" name="survey_show_results" value="" {% if not id.survey_show_results %}checked{% endif %}>
			        			{_ Thank you text only _}
							</label>
							<label class="radio">
			        			<input type="radio" name="survey_show_results" value="2" {% if id.survey_show_results == 2 %}checked{% endif %}>
			        			{_ Results from the respondent _}
							</label>
							<label class="radio">
			        			<input type="radio" name="survey_show_results" value="1" {% if id.survey_show_results == 1 %}checked{% endif %}>
			        			{_ Aggregated results from all respondents (only results for multiple choice questions are shown) _}
							</label>
						</div>
					</div>

					<div class="form-group">
						<label class="control-label">{_ Test pass percentage _}</label>
						<div class="input-group">
					        <input type="number" name="survey_test_percentage" id="{{ #survey_test_percentage }}" 
					               class="form-control" value="{{ id.survey_test_percentage }}"
					               placeholder="{_ Passing score _} (0-100)"
					               min="0" max="100">
					        <span class="input-group-addon" id="basic-addon2">%</span>
					        {% validate id=#survey_test_percentage name="survey_test_percentage"
					        			type={numericality minimum=0 maximum=100}
					        %}
					    </div>
				        <p class="help-block muted">{_ This is the percentage needed to pass a quiz or test. Only used if you have added quiz or test questions. _}</p>
					</div>
				</div>
			{% else %}
				<input type="hidden" name="survey_show_results" id="survey_show_results" value="1" />
			{% endif %}
		</div>

	</fieldset>


	<div class="row">
		<fieldset class="col-sm-6">
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

		<fieldset class="col-sm-6">
			<legend style="margin-bottom: 0">{_ Handling _}</legend>

			<div class="form-group label-floating">
				<input type="email" multiple class="form-control" name="survey_email" id="survey_email" value="{{ id.survey_email }}" placeholder="{_ Mail results to _}" />
				<label class="control-label">{_ Mail results to _}</label>
				<p class="help-block">
					{_ Separate multiple email addresses with a comma. _}
				</p>
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
	</div>
{% endblock %}
