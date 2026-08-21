{% extends "email_base.tpl" %}

{% block title %}{% if is_result_email %}{_ New result: _} {% endif %}{{ id.title }}{% endblock %}

{% block body %}

{% if is_result_email %}
	<div style="border: 1px solid #ccc; padding: 10px; margin-bottom: 16px; background-color: #eee;">
		<p>
			{_ This is a result for: _} <a href="{{ id.page_url_abs }}">{{ id.title }}</a>
			{% if respondent_id %}
				<br>
				{% if user_id and respondent_id /= user_id %}
					{_ It was filled in for: _} <a href="{{ respondent_id.page_url_abs }}">{% include "_name.tpl" id=respondent_id %}</a>
					({_ by _} {% include "_name.tpl" id=user_id %})
				{% else %}
					{_ It was filled in by: _} <a href="{{ respondent_id.page_url_abs }}">{% include "_name.tpl" id=respondent_id %}</a>
				{% endif %}
			{% endif %}
		</p>
		<p>
			{_ You cannot reply to this email. _}
		</p>
		{% block edit_answer %}
			<p><a href="{% url admin_edit_rsc id=id absolute_url %}">{_ Check the answer in the admin. _}</a></p>
		{% endblock %}
	</div>
{% endif %}

{% block feedback %}
	{% if not is_result_email %}
		{% if id.email_text_html %}
			{{ id.email_text_html|show_media:"email/_body_media.tpl" }}
		{% else %}
			<p>{_ The following has been filled in: _} <a href="{{ id.page_url_abs }}">{{ id.title }}</a></p>
		{% endif %}
	{% endif %}
{% endblock %}

{% with id|survey_test_max_points as max_points %}

{% block test_result %}
	{% if max_points and id.survey_test_percentage and result %}
    	{% with result.points >= max_points * (id.survey_test_percentage / 100) as is_passed %}
	        <h2>
	        	<br>
	            {{ (result.points / max_points * 100)|round }}% &ndash;
	            {% if is_passed %}
	                {_ Passed _}
	            {% else %}
	                {_ Failed _}
	            {% endif %}
	        </h2>

	        <table class="table" style="width: auto">
	            <tr style="border-top: 1px solid #ccc">
	                <td style="text-align: left; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{_ Points _}</td>
	                <th valign="top" style="text-align: right; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{{ result.points }} / {{ max_points }}</th>
	            </tr>
	            <tr style="border-top: 1px solid #ccc">
	                <td style="text-align: left; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{_ Needed for pass _}</td>
	                <th valign="top" style="text-align: right; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{{ id.survey_test_percentage }}%</th>
	            </tr>
	            <tr style="border-top: 1px solid #ccc">
	                <td style="text-align: left; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{_ Your result _}</td>
	                <th valign="top" style="text-align: right; padding: 4px; vertical-align: top; border-top: 1px solid #dddddd;">{{ (result.points / max_points * 100)|round }}%</th>
	            </tr>
	        </table>

	        <p>
	        	<br/>
	        	<br/>
	        </p>
        {% endwith %}
    {% endif %}
{% endblock %}

{# Check email answers setting for result email #}
{% if is_result_email
	  or id.survey_email_answers|default:0 /= 3
%}
{# For tests, also follow the survey_show_results setting #}
{% if is_result_email
	or max_points == 0
	or id.survey_show_results|default:0 /= 3
	or (
			id.survey_show_results|default:0 == 3
		and id.survey_test_percentage
		and result
		and result.points >= max_points * (id.survey_test_percentage / 100)
	)
%}
	{% with is_result_email
			or not id.survey_email_answers
			or (id.survey_email_answers == 1 and m.acl.user)
	   as include_open_questions
	%}
	<table style="width: 100%; border-collapse: collapse; border-spacing: 0; margin-bottom: 18px;">
		<tr>
			<th style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd; max-width:45%;">{_ Question _}</th>
			<th style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">{_ Answer _}</th>
		</tr>
		{% if result %}
			{% for blk in id.blocks %}
			    {% if blk.is_hide_result %}
			        {# Nothing #}
			    {% elseif blk.type == 'header' %}
					<tr>
						<td style="padding: 8px; text-align: left;" colspan="2">
							<h2 style="margin: 0">{{ blk.header }}</h2>
						</td>
					</tr>
			    {% elseif blk.type|match:"^survey_.*"
			    	  and blk.type != 'survey_page_break'
			    	  and blk.type != 'survey_page_options'
			    	  and blk.type != 'survey_stop'
			    	  and blk.name != 'survey_feedback'
			   	%}
			   		{% if include_open_questions
			   			  or (
			   			  		blk.type != 'survey_short_answer'
			   			  	and blk.type != 'survey_long_answer'
			   			  )
			   		%}
					<tr style="border-top: 1px solid #ccc">
						<td valign="top" style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd; max-width:45%;">
							{% if blk.prompt %}
								{{ blk.prompt }}
							{% else %}
								{{ blk.name|force_escape }}
							{% endif %}
						</td>
						<td>
						    {% if blk.type == 'survey_narrative' %}
								{% optional include "blocks/_block_view_"++blk.type++".tpl" blk=blk is_survey_answer_view result=result %}
						    {% else %}
						    	{% with answers[blk.name] as ans %}
		                            {% for ans in ans.answers %}
		                                {{ ans.text|linebreaksbr }}{% if blk.is_test %}{% if ans.is_correct|is_defined %}{% if ans.is_correct %} <span style="color:green;font-weight:bold">√ {_ Correct _}</span>{% else %} <span style="color:red;font-weight:bold">X {_ Wrong _}</span>{% endif %}{% endif %}{% endif %}{% if not forloop.last %}<br>{% endif %}
		                            {% endfor %}
		                        {% endwith %}
						    {% endif %}
						</td>
					</tr>
					{% endif %}
				{% endif %}
			{% endfor %}
		{% else %}
			{% for blk in id.blocks %}
				{% with blk.name as name %}
				{% with answers[name] as ans %}
				    {% if blk.is_hide_result %}
				        {# Nothing #}
				    {% elseif blk.type == 'header' %}
						<tr>
							<td style="padding: 8px; text-align: left;" colspan="2">
								<h2 style="margin: 0">{{ blk.header }}</h2>
							</td>
						</tr>
					{% elseif ans %}
				   		{% if include_open_questions
				   			  or (
				   			  		blk.type != 'survey_short_answer'
				   			  	and blk.type != 'survey_long_answer'
				   			  )
				   		%}
						<tr style="border-top: 1px solid #ccc">
							<td valign="top" style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd; max-width:45%;">
								{% if ans.question.prompt %}
									{{ ans.question.prompt }}
								{% else %}
									{{ name|force_escape }}
								{% endif %}
							</td>
							<td valign="top" style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">
								{% if blk.type == 'survey_narrative' %}
						            {% optional include "blocks/_block_view_survey_narrative.tpl" blk=blk is_survey_answer_view result=answers %}
					            {% else %}
		                            {% for ans in ans.answers %}
		                                {{ ans.text|linebreaksbr }}{% if blk.is_test %}{% if ans.is_correct|is_defined %}{% if ans.is_correct %} <span style="color:green;font-weight:bold">√ {_ Correct _}</span>{% else %} <span style="color:red;font-weight:bold">X {_ Wrong _}</span>{% endif %}{% endif %}{% endif %}{% if not forloop.last %}<br>{% endif %}
		                            {% endfor %}
								{% endif %}
							</td>
						</tr>
						{% endif %}
					{% endif %}
				{% endwith %}
				{% endwith%}
			{% endfor %}
		{% endif %}
	</table>
	{% endwith %}
{% endif %}
{% endif %}

{% endwith %}

{% endblock %}
