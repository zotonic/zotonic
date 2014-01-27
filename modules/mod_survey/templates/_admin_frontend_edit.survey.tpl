{% extends "_admin_frontend_edit.tpl" %}

{% block edit_blocks %}

<ul class="nav nav-tabs">
  <li class="active"><a href="#survey-basics" data-toggle="tab">{_ Description _}</a></li>
  <li><a href="#survey-settings" data-toggle="tab">{_ Settings _}</a></li>
  <li><a href="#survey-questions" data-toggle="tab">{_ Questions _}</a></li>
  <li><a href="#survey-results" data-toggle="tab">{_ Results _}</a></li>
</ul>


<div class="tab-content">
	 <div class="tab-pane active" id="survey-basics">
		{% catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
		{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}

		{% include "_admin_survey_edit_feedback.tpl" %}

		{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}
	 </div>

	 <div class="tab-pane" id="survey-settings">
	 	{% include "_admin_survey_settings_tab.tpl" show_opened %}
	 </div>

	 <div class="tab-pane" id="survey-questions">
	 	{% include "_admin_survey_question_editor.tpl" id=id is_editable=is_editable languages=languages %}
	 </div>

	 <div class="tab-pane" id="survey-results">
		<div class="control-group">
			{% if m.survey.is_allowed_results_download[id] %}
				<a id="{{ #download }}" class="btn btn-mini" href="{% url survey_results_download id=id %}">{_ Download CSV results _}</a>
				{% wire id=#download propagate 
						action={alert text=_"Download will start in the background. Please check your download window."}
				%}
			{% endif %}
			<a class="btn btn-mini" href="{% url survey_results id=id %}">{_ Show survey results _}</a>
			<a class="btn btn-mini" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
			{% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
			<a class="btn btn-mini" href="{% url survey_results_printable id=id %}" target="_blank">{_ Printable list _}</a>
			<!-- <a class="btn btn-mini" href="{% url admin_survey_editor id=id %}">{_ Survey results editor _}</a> -->
		</div>
	 </div>
</div>

{% javascript %}
	window.z_survey_editor = new ZSurveyEditor();
{% endjavascript %}

{% wire name="insert-block" 
		delegate=`survey_admin`
		postback={insert_block id=id}
%}

{% endblock %}

