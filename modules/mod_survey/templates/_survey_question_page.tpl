{% block question_page %}
	{% wire id=#q type="submit" 
		postback={survey_next id=id page_nr=page_nr answers=answers history=history element_id=element_id|default:"survey-question"}
		delegate="mod_survey" 
	%}
	<form class="form-survey survey-{{ id.name }}" id="{{ #q }}" method="post" action="postback">
		<fieldset>
			{% if not id.is_a.poll and pages > 1 %}
				{% if id.survey_progress == 'nr' %}
					<legend>{{ page_nr }}<span class="total">/{{ pages }}</span></legend> 
				{% elseif id.survey_progress == 'bar' %}
					<div class="progress">
					  <div class="bar" style="width: {{ page_nr * 100 / pages }}%;"></div>
					</div>
				{% endif %}
			{% endif %}

			{% for blk in questions %}
				{% include ["blocks/_block_view_",blk.type,".tpl"]|join id=id blk=blk answers=answers nr=forloop.counter %}
			{% endfor %}
		</fieldset>

		<div class="alert alert-error z_invalid">
			{_ Please fill in all the required fields. _}
		</div>

		<div class="form-actions">
			{% if page_nr > 1 %}
				<a id="{{ #back }}" href="#" class="btn">{_ Back _}</a>
				{% wire id=#back 
						postback={survey_back id=id page_nr=page_nr answers=answers history=history element_id=element_id|default:"survey-question"}
						delegate="mod_survey"
				%}
			{% endif %}
			{% if not id.is_a.poll %}
				<a id="{{ #cancel }}" href="#" class="btn">{_ Stop _}</a>
				{% wire id=#cancel action={confirm text=_"Are you sure you want to stop?" ok=_"Stop" cancel=_"Continue" action={redirect id}} %}
			{% endif %}
			{% with questions|last as last_q %}
			{% if not questions|survey_is_submit and last_q.type /= "survey_stop" %}
				<button type="submit" class="btn btn-primary">{% if page_nr == pages %}{_ Submit _}{% else %}{_ Next _}{% endif %}</button>
			{% endif %}
			{% endwith %}
		</div>
	</form>
	{% javascript %}
		$('body').removeClass('survey-start').addClass('survey-question');

		var pos = $('#{{ #q }}').position();
		if (pos.top < $(window).scrollTop() + 100) {
			$(window).scrollTop(pos+100);
		}
	{% endjavascript %}
{% endblock %}

{% block question_page_after %}
{% endblock %}
