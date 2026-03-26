{% block question_page %}
	{% if page_nr == 1 and id.survey_is_autostart %}
		{% block autostart_body %}
			{% include "_survey_autostart_body.tpl" %}
		{% endblock %}
	{% endif %}

    {% if id.survey_is_disabled and id.is_editable %}
		<p class="alert alert-info">{_ This form is disabled. Because you can edit, you can proceed for testing. _}</p>
    {% endif %}

	{% wire id=#q type="submit"
		postback={survey_next
			id=id
			page_nr=page_nr
			is_feedback_view=is_feedback_view
			feedback_submitter=feedback_submitter
			answers=answers
			answers_novalidate=answers_novalidate
			answer_user_id=answer_user_id
			history=history
			editing=editing
			element_id=element_id|default:"survey-question"
			viewer=viewer
			survey_session_nonce=survey_session_nonce
		}
		delegate="mod_survey"
	%}
	<form class="form-survey survey-{{ id.name }}" id="{{ #q }}" data-id="{{ id }}" method="post" action="postback">
		<fieldset>
			{% if not id.is_a.poll and pages > 1 %}
                {% if id.survey_progress == 'nr' %}
                    <legend>
                        {% if id.is_survey_non_linear and page_nr > 1 %}
                            <button class="btn btn-default btn-sm" name="z_survey_start" type="submit">{_ &lt; Back to start _}</button>
                        {% endif %}
                        {{ page_nr }}<span class="total">/{{ pages }}</span>
                    </legend>
                {% elseif id.survey_progress == 'bar' %}
                    <div class="progress">
                      <div class="bar" style="width: {{ page_nr * 100 / pages }}%;"></div>
                    </div>
                {% endif %}

                {% if page_nr > 1 and id.is_survey_non_linear and id.survey_progress /= 'nr' %}
                    <button class="btn btn-default btn-sm" name="z_survey_start" type="submit">{_ &lt; Back to start _}</button>
                {% endif %}
			{% endif %}

			{% block before_questions %}
			{% endblock %}

			{% if is_feedback_view %}
				{% for blk in questions %}
					{% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
								id=id
								blk=blk
								result=feedback_result
	                            answer_user_id=answer_user_id|default:m.acl.user
								editing=editing
								nr=forloop.counter
								is_survey_answer_view
					%}
				{% endfor %}
			{% else %}
				{% with answers ++ answers_novalidate as answers_prefill %}
					{% for blk in questions %}
						{% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
									id=id
									blk=blk
									answers=answers_prefill
		                            answer_user_id=answer_user_id|default:m.acl.user
									editing=editing
									nr=forloop.counter
						%}
					{% endfor %}
				{% endwith %}
			{% endif %}

			{% block after_questions %}
			{% endblock %}
		</fieldset>

		<div class="alert alert-danger z_invalid">
			{% if id.is_survey_non_linear and id|survey_is_history_back:history %}
				{_ Not all the required fields are filled in. You can go back to the previous page without saving your answers. _}
				<br>
				<br>
				<button class="btn btn-default" formnovalidate type="submit">{_ Back without saving _}</button>
			{% else %}
				{_ Please fill in all the required fields. _}
			{% endif %}
		</div>

		{% if editing and pages == 1 %}
			<div class="modal-footer">
		{% else %}
			<div class="form-actions">
		{% endif %}

		{% if is_feedback_view %}
			<button class="btn btn-primary btn-lg survey-next" type="submit">{_ Continue _}</button>
		{% else %}
			{% with questions|survey_page_options as options %}

				{% if id|survey_is_history_back:history %}
					{% if id.is_survey_non_linear %}
						<button class="btn btn-default" name="z_survey_back" type="submit">{_ Back _}</button>
					{% else %}
						<button class="btn btn-default" formnovalidate type="submit">{_ Back _}</button>
					{% endif %}
				{% endif %}

				{% if not editing or pages > 1 %}
					{% if not id.survey_is_autostart or page_nr > 1 %}
						{% with (viewer == 'overlay')|if
									:{overlay_close}
									:(
										(viewer == 'dialog')|if
											:{dialog_close}
											:{redirect id=id}
									)
							as close_action
						%}
							{% if id|survey_is_save_intermediate %}
								{# Save the answers to the intermediate answers, and then show dialog if user wants to stop. #}
								<button id="{{ #save }}" class="btn btn-default" name="z_survey_save" formnovalidate type="submit" title="{_ Save your answers without submitting them yet so you can continue later. _}">
									{_ Continue later _}
								</button>
								{% wire name="survey-stop-confirm"
										action={confirm
											text=[
												_"Do you want to stop for now and continue later?",
												"<br>",
												_"Your progress has been saved. You can return later to finish and submit it."
											]
											ok=_"Stop and continue later"
											cancel=_"Cancel"
											action=close_action
										}
								%}
								{% wire name="survey-close" action=close_action %}
							{% else %}
								{# No intermediate answers saved, directly ask user if they want to stop. #}
								<button id="{{ #cancel }}" class="btn btn-default" type="button">{_ Stop _}</button>
								{% wire id=#cancel
										action={confirm
											text=_"Are you sure you want to stop without saving?"
											ok=_"Stop without saving"
											cancel=_"Cancel"
											action=close_action
										}
								%}
							{% endif %}
						{% endwith %}
					{% endif %}
				{% else %}
					<button id="{{ #cancel }}" class="btn btn-lg btn-default" type="button">{_ Cancel _}</button>
					{% wire id=#cancel action={dialog_close} %}
				{% endif %}

				{% if editing %}
					<button type="submit" class="btn btn-primary btn-lg">{% if page_nr == pages %}{_ Submit _}{% else %}{_ Next _}{% endif %}</button>
				{% elseif not options.is_stop_page and not questions|survey_is_submit %}
					{% if page_nr == pages or questions|survey_is_pagebreak_submit %}
						<button type="submit" class="btn btn-primary btn-lg survey-submit">{_ Submit _}</button>
					{% else %}
						<button type="submit" class="btn btn-primary btn-lg survey-next">{_ Next _}</button>
					{% endif %}
				{% endif %}

			{% endwith %}
		{% endif %}
		</div>
	</form>
	{% javascript %}
		$('body').removeClass('survey-start').addClass('survey-question');

        {% if viewer == 'overlay' %}
            if ($(".overlay-content-wrapper").length > 0) {
            	$(".overlay-content-wrapper").get(0).scrollTo(0, 0);
            }
        {% elseif viewer == 'dialog' %}
        {% else %}
            var pos = $('#{{ #q }}').position();
            if (pos.top < $(window).scrollTop() + 100) {
                $(window).scrollTop(pos+100);
            }
        {% endif %}
	{% endjavascript %}

	{% if page_nr == 1 and id.survey_is_autostart %}
		{% block autostart_footer %}
			{% optional include "_survey_autostart_footer.tpl" %}
		{% endblock %}
	{% endif %}
{% endblock %}

{% block question_page_after %}
{% endblock %}
