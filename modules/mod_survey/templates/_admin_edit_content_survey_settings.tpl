{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}{_ Survey _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey{% endblock %}

{% block widget_before %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block widget_content %}
<div class="row-fluid">
	<div class="span6">
		<div class="control-group">
            <div class="checkbox">
            	<label>
		            <input type="checkbox" name="survey_is_autostart" id="survey_is_autostart" value="1" {% if id.survey_is_autostart or (id.survey_is_autostart|is_undefined and id.is_a.poll) %}checked="checked"{% endif %} />
		            {_ Immediately start with the questions, no “Start” button _}
	            </label>
	        </div>
			<div class="checkbox">
				<label>
					<input type="checkbox" name="survey_email_respondent" id="survey_email_respondent" value="1" {% if id.survey_email_respondent  %}checked{% endif %} />
					{_ Send confirmation to respondent (please add a question named <i>email</i>) _}
				</label>
			</div>
		</div>

		<div class="control-group">
        	<label class="control-label">{_ Fill in _}</label>
        	<div class="controls">
				<select name="survey_multiple" id="survey_multiple">
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
	</div>

	<div class="span6">
		<div class="pull-right">
			<a href="javascript:void(0)" class="btn btn-mini btn-primary do_dialog" data-dialog="title: '{{ _"Help about surveys"|escapejs }}', text: '{{ _"You can create your survey by adding blocks with questions below the body."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
		</div>

		<div class="control-group">
			<label class="radio">
				<input type="radio" name="survey_progress" id="survey_progress_none" value="" {% if not id.survey_progress %}checked="checked"{% endif %} />
				{_ Hide progress information _}
			</label>
			<label class="radio">
				<input type="radio" name="survey_progress" id="survey_progres_nr" value="nr" {% if id.survey_progress == 'nr' %}checked="checked"{% endif %} />
				{_ Show progress information as “<em>Question 3/10</em>” _}
			</label>
			<label class="radio">
				<input type="radio" name="survey_progress" id="survey_progress_bar" value="bar" {% if id.survey_progress == 'bar' %}checked="checked"{% endif %} />
				{_ Show progress bar _}
			</label>
		</div>
	</div>
</div>

{% if not id.is_a.poll %}
	<div class="control-group">
		<label class="control-label">{_ When finished show _}</label>
		<div class="controls">
			<select name="survey_show_results" id="survey_show_results">
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

<div class="control-group">
	<label class="control-label">{_ Mail filled in surveys to _}</label>
	<div class="controls">
		<input type="text" class="input-large" name="survey_email" id="survey_email" value="{{ id.survey_email }}" />
	</div>
</div>

{% if m.survey.handlers|length %}
<div class="control-group">
	<label class="control-label">{_ Handle this survey with _}</label>
	<div class="controls">
		<select name="survey_handler" id="survey_handler">
			<option value=""></option>
			{% for val,desc in m.survey.handlers %}
				<option value="{{ val }}" {% if id.survey_handler == val %}selected{% endif %}>{{ desc }}</option>
			{% endfor %}
		</select>
	</div>
</div>
{% endif %}

<div class="control-group">
	{% if m.survey.is_allowed_results_download[id] and m.modules.active.mod_export %}
		<a id="{{ #download1 }}" class="btn btn-mini" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
		{% wire id=#download1 propagate
				action={alert text=_"Download will start in the background. Please check your download window."}
		%}
		<a id="{{ #download2 }}" class="btn btn-mini" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
		{% wire id=#download2 propagate
				action={alert text=_"Download will start in the background. Please check your download window."}
		%}
	{% endif %}
	<a class="btn btn-mini" href="{% url survey_results id=id %}">{_ Show survey results _}</a>
	<a class="btn btn-mini" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
	{% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
	<a class="btn btn-mini" href="{% url survey_results_printable id=id %}">{_ Printable list _}</a>
	<a class="btn btn-mini" href="{% url admin_survey_editor id=id %}">{_ Survey results editor _}</a>
</div>
{% endblock %}
