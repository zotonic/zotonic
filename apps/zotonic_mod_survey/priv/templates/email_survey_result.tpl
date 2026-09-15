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
		{# Inline styles and table layout keep the result summary compatible with email clients. #}
		<table class="table" width="100%" cellspacing="0" cellpadding="0" style="width: 100%; border-collapse: collapse; margin: 24px 0; background-color: #f3f6fa; color: #243247; border: 1px solid #d8e1ec;">
			<tr>
				<td colspan="2" style="padding: 20px; border-top: 4px solid {% if is_passed %}#28734a{% else %}#a33a32{% endif %};">
					<h2 style="margin: 0; font-size: 28px; line-height: 36px; color: {% if is_passed %}#28734a{% else %}#a33a32{% endif %};">
						{{ (result.points / max_points * 100)|round }}% &ndash;
						{% if is_passed %}
							{_ Passed _}
						{% else %}
							{_ Failed _}
						{% endif %}
					</h2>
				</td>
			</tr>
			<tr>
				<th scope="row" style="text-align: left; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: normal;">{_ Points _}</th>
				<td style="text-align: right; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: bold;">{{ result.points }} / {{ max_points }}</td>
			</tr>
			<tr>
				<th scope="row" style="text-align: left; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: normal;">{_ Needed for pass _}</th>
				<td style="text-align: right; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: bold;">{{ id.survey_test_percentage }}%</td>
			</tr>
			<tr>
				<th scope="row" style="text-align: left; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: normal;">{_ Your result _}</th>
				<td style="text-align: right; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: bold;">{{ (result.points / max_points * 100)|round }}%</td>
			</tr>
			<tr>
				<th scope="row" style="text-align: left; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec; font-weight: normal;">{_ Submitted _}</th>
				<td style="text-align: right; padding: 12px 20px; vertical-align: top; border-top: 1px solid #d8e1ec;">
					{{ result.created|date:_"Y-m-d H:i" }}
					{% if result.modified > result.created %}
						<span style="font-size: 13px;">({_ modified _} {{ result.modified|date:_"Y-m-d H:i" }})</span>
					{% endif %}
				</td>
			</tr>
		</table>
		{% endwith %}
	{% endif %}
{% endblock %}

{# Check email answers setting for result email #}
{% if is_result_email
	  or include_editor_only_answers
	  or id.survey_email_answers|default:0 /= 3
%}
{# For tests, also follow the survey_show_results setting #}
{% if is_result_email
	or include_editor_only_answers
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
						{% if blk.is_editor_only
							or include_open_questions
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
					{% if blk.is_editor_only
						or include_open_questions
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
