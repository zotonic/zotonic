{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey (form) #}


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
{% with m.survey.handlers as handlers %}

 	<fieldset>
 		<h4>{_ Answering _}</h4>
		<div class="form-group">
            <label class="checkbox">
	            <input type="checkbox" name="survey_is_autostart" id="survey_is_autostart" value="1" {% if id.survey_is_autostart or (id.survey_is_autostart|is_undefined and id.is_a.poll) %}checked="checked"{% endif %}>
		            {_ Immediately start with the questions, no “Start” button _}
	        </label>
			<label class="checkbox">
				<input type="checkbox" name="survey_anonymous" id="survey_anonymous" value="1" {% if id.survey_anonymous %}checked="checked"{% endif %}> {_ Hide the user’s id or browser-id from result exports _}
			</label>
        	<label class="checkbox">
                <input type="checkbox" name="survey_is_disabled" id="survey_is_disabled" value="1" {% if id.survey_is_disabled %}checked="checked"{% endif %}>
                {_ Deactivated _}
            </label>
		</div>

		{% block survey_settings_answering %}{% endblock %}
	</fieldset>

	<hr>

	<fieldset>
    	<h4>{_ Filling in _}</h4>
		<div class="form-group">
			<p class="help-block">{_ Choose how many times a respondent can fill in this form. _} {_ 'Once' means once by a respondent who is logged in or once per browser for anonymous respondents. To identify the browser of anonymous respondents, a cookie containing a unique ID is utilized. _}</p>
        	<div class="controls">
        		<label class="radio">
        			<input type="radio" name="survey_multiple" value="0" {% if not id.survey_multiple %}checked{% endif %}>
        			{_ Once (no editing after submit) _}
        		</label>
        		<label class="radio">
        			<input type="radio" name="survey_multiple" value="1" {% if id.survey_multiple == 1 %}checked{% endif %}>
        			{_ Multiple times – each time with new results _}
        		</label>
        		<label class="radio">
        			<input type="radio" name="survey_multiple" value="3" {% if id.survey_multiple == 3 %}checked{% endif %}>
        			{_ Once — save halfway and continue later + submit is final (no editing) _}
        		</label>
        		<label class="radio">
        			<input type="radio" name="survey_multiple" value="2" {% if id.survey_multiple == 2 %}checked{% endif %}>
        			{_ Once — save halfway and continue later + edit after submit (logged in users only) _}
        		</label>
			</div>

			{% block survey_settings_fill %}{% endblock %}

        	<br>

			<h5>{_ Maximum number of submissions _}</h5>
	        <p class="help-block">
	        	{_ If the maximum number of forms has been submitted, then the start button is deactivated and an email is sent to the email addresses in the <em>handling</em> settings below. Leave empty for no limit. This is useful for signup lists with a limited number of spots. _} {_ This is useful for signup lists with a limited number of spots. _}
	        </p>

			<div class="label-floating">
		        <input type="number" name="survey_max_results_int" id="{{ #survey_max_results }}" 
		               class="form-control" value="{{ id.survey_max_results_int }}"
		               placeholder="{_ Maximum number of submissions _}"
		               min="1"
		               style="max-width: 40ch">
		        {% validate id=#survey_max_results name="survey_max_results_int"
		        			type={numericality minimum=1}
		        %}
		        <label for="{{ #survey_max_results }}">
		        	{_ Maximum number of submissions _}
		        </label>
		    </div>

			{% if handlers %}
				<p class="help-block">
					<br>
					{_ <b>Note</b>: For the features 'once', 'edit after submit', and checking the maximum number of submitted forms to function, the form needs to have the saved results. If a handler (chosen in the <em>handling</em> settings below) does not save the results, these features will not be available. _} {_ The 'continue later' feature operates independently of the chosen form handler. _}
				</p>
			{% endif %}

		</div>

		{% block survey_settings_filling_in %}{% endblock %}
	</fieldset>

	{% if not id.is_a.poll %}
		<hr>

		<fieldset>
			<h4>{_ Show after the final submit of the form _}</h4>
			<div class="form-group">
				<p class="help-block">
					{_ After all questions are answered and submitted, one of the following is shown. _}
					{% if handlers %}
						{_ Note that if you select a special handler below, then that handler may override this setting. _}
					{% endif %}
				</p>
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
	        			<input type="radio" name="survey_show_results" value="3" {% if id.survey_show_results == 3 %}checked{% endif %}>
	        			{_ Results from the respondent – if passed only_}
					</label>
					<label class="radio">
	        			<input type="radio" name="survey_show_results" value="1" {% if id.survey_show_results == 1 %}checked{% endif %}>
	        			{_ Aggregated results from all respondents _}
					</label>
				</div>
			</div>

			{% block survey_settings_finished %}{% endblock %}
		</fieldset>
	{% else %}
		<input type="hidden" name="survey_show_results" id="survey_show_results" value="1">
	{% endif %}

	<hr>

	<fieldset>
		<h4>{_ Progress _}</h4>
		<div class="form-group">
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progress_none" value="" {% if not id.survey_progress %}checked="checked"{% endif %}>
					{_ Hide progress information _}
				</label>
			</div>
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progres_nr" value="nr" {% if id.survey_progress == 'nr' %}checked="checked"{% endif %}>
					{_ Show progress information as “<em>Question 3/10</em>” _}
				</label>
			</div>
			<div class="radio">
				<label>
					<input type="radio" name="survey_progress" id="survey_progress_bar" value="bar" {% if id.survey_progress == 'bar' %}checked="checked"{% endif %}>
					{_ Show progress bar _}
				</label>
			</div>
		</div>

		{% block survey_settings_progress %}{% endblock %}
	</fieldset>

	{% if not id.is_a.poll %}
		<hr>
		<fieldset>
			<h4>{_ Test pass percentage _}</h4>
	        <p class="help-block">{_ This is the percentage needed to pass a quiz or test. Only used if you have added quiz or test questions. _}</p>
			<div class="form-group">
				<div class="input-group" style="max-width: 12ch">
			        <input type="number" name="survey_test_percentage" id="{{ #survey_test_percentage }}" 
			               class="form-control" value="{{ id.survey_test_percentage }}"
			               placeholder="{_ Passing score _} (0-100)"
			               min="0" max="100">
			        <span class="input-group-addon" id="basic-addon2">%</span>
			        {% validate id=#survey_test_percentage name="survey_test_percentage"
			        			type={numericality minimum=0 maximum=100}
			        %}
			    </div>
			</div>
		</fieldset>
	{% endif %}

	<hr>

	<fieldset>
		<h4>{_ Handling _}</h4>

		<div class="form-group label-floating">
			<input type="email" multiple class="form-control" name="survey_email" id="survey_email" value="{{ id.survey_email }}" placeholder="{_ Mail results to _}">
			<label class="control-label">{_ Mail results to _}</label>
			<p class="help-block">
				{_ Separate multiple email addresses with a comma. _}
			</p>
		</div>

		<div class="form-group">
			<div class="checkbox">
				<label>
					<input type="checkbox" name="survey_email_respondent" id="survey_email_respondent" value="1" {% if id.survey_email_respondent  %}checked{% endif %}>
					{_ Send a confirmation to the respondent. The email address must be known (logged in respondents) or you can add a short text field with the label “email” and the validation set to "must be an email address". _}
				</label>
			</div>
		</div>

		{% if handlers %}
			<div class="form-group">
				<label class="control-label">{_ Handle this form with _}</label>
				<div>
					<select class="form-control" name="survey_handler" id="survey_handler">
						<option value=""></option>
						{% for val,desc in handlers %}
							<option value="{{ val }}" {% if id.survey_handler == val %}selected{% endif %}>{{ desc }}</option>
						{% endfor %}
					</select>
				</div>
			</div>
		{% endif %}

		{% block survey_settings_handling %}{% endblock %}
	</fieldset>
{% endwith %}
{% endblock %}
