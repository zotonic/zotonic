<h2>{_ Question _} <span class="progress">{{ page_nr }}<span class="total">/{{ pages }}</span></h2> 

{% wire id=#q type="submit" postback={survey_next id=id page_nr=page_nr answers=answers} delegate="mod_survey" %}
<form id="{{ #q }}" method="post" action="postback">
	{% for question in questions %}
	{% with question.type|make_list as t %}
		<fieldset class="{{ t }}">
			{% if t == "subhead" %}
				{% include "_survey_question_subhead.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "textblock" %}
				{% include "_survey_question_textblock.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "prompt" %}
				{% include "_survey_question_prompt.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "yesno" %}
				{% include "_survey_question_yesno.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "truefalse" %}
				{% include "_survey_question_truefalse.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "likert" %}
				{% include "_survey_question_likert.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "thurstone" %}
				{% include "_survey_question_thurstone.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "shortanswer" %}
				{% include "_survey_question_shortanswer.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "longanswer" %}
				{% include "_survey_question_longanswer.tpl" question=question name=question.name %}
			{% endif %}
			{% if t == "narrative" %}
				{% include "_survey_question_narrative.tpl" question=question name=question.name %}
			{% endif %}
		</fieldset>
	{% endwith %}
	{% endfor %}

	<div class="buttons">
		{% if page_nr > 1 %}
			<a id="{{ #back }}" href="#">&lt; {_ Back _}</a>
		{% else %}
			<a id="{{ #back }}" href="#">&lt; {_ Cancel _}</a>
		{% endif %}
		{% wire id=#back postback={survey_back id=id page_nr=page_nr answers=answers} delegate="mod_survey" %}
		<button type="submit" class="submit next">{_ Next _} &gt;</button>
	</div>
</form>

