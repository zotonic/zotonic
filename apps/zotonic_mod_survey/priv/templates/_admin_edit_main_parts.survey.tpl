
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

        {% if id.category_id.is_feature_show_address|if_undefined:true %}
            {% catinclude "_admin_edit_content_address.tpl" id %}
        {% endif %}

        {% if id.category_id.is_feature_show_geodata
                 |if_undefined:(id.category_id.is_feature_show_address)
                 |if_undefined:true
              or id.location_lat
              or id.location_lng %}
            {% optional include "_geomap_admin_location.tpl" %}
        {% endif %}

        {% if id.is_a.media or id.medium %}
            {% include "_admin_edit_content_media.tpl" %}
        {% endif %}

        {% catinclude "_admin_edit_depiction.tpl" id %}

        {% catinclude "_admin_edit_content_advanced.tpl" id show_header %}
        {% optional include "_admin_edit_content_seo.tpl" show_header %}
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
        {% include "_admin_survey_results_tab.tpl" show_opened %}
        {% lazy template="_admin_survey_results_charts.tpl" id=id %}
    </div>
</div>

{% lib "js/admin_survey_question_editor.js" %}
{% lib "css/admin_survey.css" %}

{% wire name="insert-block"
        delegate=`survey_admin`
        postback={insert_block id=id}
%}

{% javascript %}
    window.z_survey_editor = new ZSurveyEditor();

    window.z_translations = window.z_translations || {};
    window.z_translations["OK"] = "{_ OK _}";
    window.z_translations["Cancel"] = "{_ Cancel _}";
    window.z_translations["Delete"] = "{_ Delete _}";
    window.z_translations["Confirm"] = "{_ Confirm _}";
    window.z_translations["Are you sure you want to delete this page?<br/>This also deletes all questions on this page."] = "{_ Are you sure you want to delete this page?<br/>This also deletes all questions on this page. _}";
    window.z_translations["Are you sure you want to delete this question?"] = "{_ Are you sure you want to delete this question? _}";
    window.z_translations["Are you sure you want to delete this page jump?"] = "{_ Are you sure you want to delete this page jump? _}";
{% endjavascript %}
