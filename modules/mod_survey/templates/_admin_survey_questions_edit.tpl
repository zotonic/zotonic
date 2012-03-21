{% for q_id, question in m.survey.questions[id] %}
<li id="{{ q_id }}"> 
    <div class="pull-right">
        <button class="btn btn-mini" id="{{ #e.q_id }}">{_ Edit _}</button>
        <button class="btn btn-mini" id="{{ #x.q_id }}"><i class="icon-remove"></i></button>
    </div>
    
    <p class="survey-info">{{ question.name|escape }}</p>
    {{ question.html|replace:['class="', 'class="nosubmit '] }}
</li>
{% sortable id=q_id tag=q_id%}
{% wire id=#x.q_id delegate="mod_survey" action={dialog_survey_question_delete id=id question_id=q_id} %}
{% wire id=#e.q_id delegate="mod_survey" action={dialog_survey_question id=id question_id=q_id} %}
{% empty %}
<li>
    {_ Drag questions here. _}
</li>
{% endfor %}
