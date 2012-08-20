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
			{% if blk.name or blk.type == `survey_page_break` %}
				{% include ["blocks/_block_view_",blk.type,".tpl"]|join id=id blk=blk answers=answers %}
			{% else %}
				<p class="alert alert-error"><strong>{_ Error _}</strong> {_ You need to give a name to every question. _}</p>
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
			{% else %}
				{% if not id.is_a.poll %}
					<a id="{{ #back }}" href="#" class="btn">{_ Cancel _}</a>
					{% wire id=#back action={redirect id} %}
				{% endif %}
			{% endif %}
			{% if last.type != 'survey_button' and last.input_type != 'submit' %}
				<button type="submit" class="btn btn-primary">{% if page_nr == pages %}{_ Submit _}{% else %}{_ Next Question _}{% endif %}</button>
			{% endif %}
		</div>
	{% endwith %}
</form>

{% javascript %}
    $('body').removeClass('survey-start').addClass('survey-question');
{% endjavascript %}

