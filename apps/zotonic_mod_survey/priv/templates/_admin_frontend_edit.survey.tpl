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
		{% catinclude "_admin_edit_basics.tpl" id %}
		{% catinclude "_admin_edit_body.tpl" id property="body" explanation=_"This text is shown as an introduction to the survey." %}
    {% catinclude "_admin_edit_body.tpl" id property="body_extra" explanation=_"This text is shown below the start button." %}
		{% include "_admin_survey_edit_feedback.tpl" %}
		{% catinclude "_admin_edit_depiction.tpl" id %}
	</div>

	<div class="tab-pane" id="survey-settings">
		{% include "_admin_survey_settings_tab.tpl" show_opened %}
		<div id="tinyinit2">
			{% lazy action={script script="z_editor_init();"} action={remove target="tinyinit2"} %}
		</div>
		{% include "_admin_survey_edit_email_text.tpl" %}
	</div>

	<div class="tab-pane" id="survey-questions">
		{% include "_admin_survey_question_editor.tpl" id=id %}
	</div>

	<div class="tab-pane" id="survey-results">
		{% include "_admin_survey_results_tab.tpl" %}
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

