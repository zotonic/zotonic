{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}
{_ Survey _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Help about surveys"|escapejs }}', text: '{{ _"You can create your survey by adding blocks with questions below the body."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey{% endblock %}

{% block widget_before %}
    {% lib "css/admin_survey.css" %}
{% endblock %}

{% block widget_content %}
<fieldset class="form-horizontal">
    <div class="form-group">
	    <div class="col-lg-6 col-md-6">
            <div class="checkbox">
            	<label>
		            <input type="checkbox" name="survey_is_autostart" id="survey_is_autostart" value="1" {% if id.survey_is_autostart or (id.survey_is_autostart|is_undefined and id.is_a.poll) %}checked="checked"{% endif %} />
		            {_ Immediately start with the questions, no “Start” button _}
	            </label>
	        </div>
		    <div class="checkbox">
		    	<label>
			        {% if id.is_a.poll %}
				        <input type="hidden" name="survey_show_results" id="survey_show_results" value="1" />
				        <input type="checkbox" disabled="disabled" checked="checked" />
			        {% else %}
				        <input type="checkbox" name="survey_show_results" id="survey_show_results" value="1" {% if id.survey_show_results %}checked="checked"{% endif %} />
			        {% endif %}
			        {_ Show results to user after completion (only results for multiple choice questions are shown) _}
		        </label>
		    </div>
            <div class="checkbox">
            	<label>
		            <input type="checkbox" name="survey_multiple" id="survey_multiple" value="1" {% if id.survey_multiple or id.survey_multiple|is_undefined %}checked="checked"{% endif %} />
		            {_ Allow multiple entries per user/browser _}
	            </label>
	       	</div>
            <div class="checkbox">
            	<label>
		            <input type="checkbox" name="survey_email_respondent" id="survey_email_respondent" value="1" {% if id.survey_email_respondent %}checked="checked"{% endif %} />
		            {_ Send confirmation to respondent (please add a question named <i>email</i>) _}
	            </label>
	        </div>
	    </div>

	    <div class="col-lg-6 col-md-6">
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
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-6">{_ Mail filled in surveys to _}</label>
	    <div class="col-md-6">
		    <input type="text" class="form-control" name="survey_email" id="survey_email" value="{{ id.survey_email }}" />
	    </div>
    </div>

    {% if m.survey.handlers|length %}
        <div class="form-group row">
	        <label class="control-label col-md-6">{_ Handle this survey with _}</label>
	        <div class="col-md-6">
		        <select class="form-control" name="survey_handler" id="survey_handler">
			        <option value=""></option>
			        {% for val,desc in m.survey.handlers %}
				        <option value="{{ val }}" {% if id.survey_handler == val %}selected{% endif %}>{{ desc }}</option>
			        {% endfor %}
		        </select>
	        </div>
        </div>
    {% endif %}

    <div class="form-group row">
	    <div class="col-lg-12 col-md-12">
		    {% if m.survey.is_allowed_results_download[id] %}
				<a id="{{ #download1 }}" class="btn btn-default btn-xs" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
				{% wire id=#download1 propagate
						action={alert text=_"Download will start in the background. Please check your download window."}
				%}
				<a id="{{ #download2 }}" class="btn btn-default btn-xs" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
				{% wire id=#download2 propagate
						action={alert text=_"Download will start in the background. Please check your download window."}
				%}
		    {% endif %}
		    <a class="btn btn-default btn-xs" href="{% url survey_results id=id %}">{_ Show survey results _}</a>
		    <a class="btn btn-default btn-xs" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
		    {% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
		    <a class="btn btn-default btn-xs" href="{% url survey_results_printable id=id %}">{_ Printable list _}</a>
		    <a class="btn btn-default btn-xs" href="{% url admin_survey_editor id=id %}">{_ Survey results editor _}</a>
		</div>
    </div>
</fieldset>
{% endblock %}

