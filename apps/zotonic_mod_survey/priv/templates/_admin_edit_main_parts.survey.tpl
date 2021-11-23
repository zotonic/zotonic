
<ul class="nav nav-tabs">
  <li class="active"><a href="#survey-basics" data-toggle="tab">{_ Description _}</a></li>
  <li><a href="#survey-settings" data-toggle="tab">{_ Settings _}</a></li>
  <li><a href="#survey-questions" data-toggle="tab">{_ Questions _}</a></li>
  <li><a href="#survey-results" data-toggle="tab">{_ Results _}</a></li>
</ul>


<div class="tab-content">
     <div class="tab-pane active" id="survey-basics">
        {% catinclude "_admin_edit_basics.tpl" id %}
        {% catinclude "_admin_edit_body.tpl" id explanation=_"This text is shown as an introduction to the survey." %}
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

        {% include "_admin_edit_content_advanced.tpl" %}
        {% include "_admin_edit_content_seo.tpl" show_header %}
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
        <div class="form-group">
            {% block survey_results %}
                {% if m.survey.is_allowed_results_download[id] and m.modules.active.mod_export %}
                    <a id="{{ #download1 }}" class="btn" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
                    {% wire id=#download1 propagate
                            action={alert text=_"Download will start in the background. Please check your download window."}
                    %}
                    <a id="{{ #download2 }}" class="btn" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
                    {% wire id=#download2 propagate
                            action={alert text=_"Download will start in the background. Please check your download window."}
                    %}
                {% endif %}
                <a class="btn" href="{% url survey_results id=id %}" target="_blank">{_ Show results _} <i class="fa fa-external-link"></i></a>
                <a class="btn" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
                {% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
                <a class="btn" href="{% url survey_results_printable id=id %}" target="_blank">{_ Printable list _} <i class="fa fa-external-link"></i></a>

                {#
                    The result editor now opens in the admin template, this needs to be changed for frontend-admin
                    <a class="btn" href="{% url admin_survey_editor id=id %}">{_ Results editor _}</a>
                #}
            {% endblock %}
        </div>
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

    $('.pages').on('click', '.block-page a.page-connect', function(event) {
        window.zBlockConnectTrigger = this;
        z_event("admin-block-connect", {});
        event.preventDefault();
    });
{% endjavascript %}
