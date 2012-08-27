{% wire id=#q type="submit" 
	postback={survey_next id=id page_nr=page_nr answers=answers history=history element_id=element_id|default:"survey-question"}
	delegate="mod_survey" 
%}
<form class="form-survey survey-{{ id.name }}" id="{{ #q }}" method="post" action="postback">
	<fieldset>
		{% if not id.is_a.poll and pages > 1 and not id.survey_hide_progress %}
			<legend>{_ Question _} <span>{{ page_nr }}<span class="total">/{{ pages }}</span></legend> 
		{% endif %}

		{% for blk in questions %}
			{% if not blk.name and blk.type != `survey_page_break` and blk.type != 'survey_stop' 
					and blk.type != 'text'and blk.type != 'header' 
			%}
				<p class="alert alert-error"><strong>{_ Error _}</strong> {_ You need to give a name to every question. _}</p>
			{% endif %}
			{% if blk.type != 'survey_stop' %}
				{% include ["blocks/_block_view_",blk.type,".tpl"]|join id=id blk=blk answers=answers %}
			{% endif %}
		{% endfor %}
	</fieldset>

	{% with questions|last as last %}
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
			{% if last.type != 'survey_button' and last.input_type != 'submit' and last.type != 'survey_stop' %}
				<button type="submit" class="btn btn-primary">{% if page_nr == pages %}{_ Submit _}{% else %}{_ Next Question _}{% endif %}</button>
			{% endif %}
		</div>
	{% endwith %}
</form>

{% javascript %}
    $('body').removeClass('survey-start').addClass('survey-question');
{% endjavascript %}

