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
		{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages
			explanation=_"This text is shown as an introduction to the survey."
		%}
		{% include "_admin_survey_edit_feedback.tpl" %}
		{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}
	 </div>

	 <div class="tab-pane" id="survey-settings">
	 	{% include "_admin_survey_settings_tab.tpl" show_opened %}
		<div id="tinyinit2">
			{% lazy action={script script="z_editor_init();"} action={remove target="tinyinit2"} %}
		</div>
		{% include "_admin_survey_edit_email_text.tpl" %}
	 </div>

	 <div class="tab-pane" id="survey-questions">
	 	{% include "_admin_survey_question_editor.tpl" id=id is_editable=is_editable languages=languages %}
	 </div>

	 <div class="tab-pane" id="survey-results">
		<div class="form-group">
			{% if m.survey.is_allowed_results_download[id] and m.modules.active.mod_export %}
				<a id="{{ #download1 }}" class="btn btn-default btn-xs" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
				{% wire id=#download1 propagate
						action={alert text=_"Download will start in the background. Please check your download window."}
				%}
				<a id="{{ #download2 }}" class="btn btn-default btn-xs" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
				{% wire id=#download2 propagate
						action={alert text=_"Download will start in the background. Please check your download window."}
				%}
			{% endif %}
			<a class="btn btn-default btn-xs" href="{% url survey_results id=id %}" target="_blank">{_ Show survey results _}</a>
			<a class="btn btn-default btn-xs" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
			{% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
			<a class="btn btn-default btn-xs" href="{% url survey_results_printable id=id %}" target="_blank">{_ Printable list _}</a>
			<!-- <a class="btn btn-default btn-xs" href="{% url admin_survey_editor id=id %}">{_ Survey results editor _}</a> -->
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

