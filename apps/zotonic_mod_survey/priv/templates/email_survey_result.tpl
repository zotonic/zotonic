{% extends "email_base.tpl" %}

{% block title %}{% if is_result_email %}{_ New result: _} {% endif %}{{ id.title }}{% endblock %}

{% block body %}

{% if is_result_email %}
	<p>{_ This is a result for: _} <a href="{{ id.page_url_abs }}">{{ id.title }}</a></p>
	{% block edit_answer %}
		{% if is_result_email %}
			<p><a href="{% url admin_edit_rsc id=id absolute_url %}">{_ Check the answer in the admin. _}</a></p>
		{% endif %}
	{% endblock %}
{% endif %}

{% block feedback %}
	{% if not is_result_email %}
		{% if id.email_text_html %}
			{{ id.email_text_html|show_media:"_body_media_mailing.tpl" }}
		{% else %}
			<p>{_ Thank you for filling in: _} <a href="{{ id.page_url_abs }}">{{ id.title }}</a></p>
		{% endif %}
	{% endif %}
{% endblock %}

{% block test_result %}
	{% if id.survey_test_percentage and result %}
	    {% with id|survey_test_max_points as max_points %}
	    	{% if max_points %}
		        <h2>
		        	<br/>
		            {{ (result.points / max_points * 100)|round }}% &ndash;
		            {% if result.points >= max_points * (id.survey_test_percentage / 100) %}
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
	        {% endif %}
	    {% endwith %}
	{% endif %}
{% endblock %}

<table style="width: 100%; border-collapse: collapse; border-spacing: 0; margin-bottom: 18px;">
	<tr>
		<th style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">{_ Question _}</th>
		<th style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">{_ Answer _}</th>
	</tr>
{% for name, ans in answers %}
	{% if not id.blocks[name].is_hide_result %}
	<tr style="border-top: 1px solid #ccc">
		<td valign="top" style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">
			{% if ans.question.prompt %}
				{{ ans.question.prompt }}
			{% else %}
				{{ name|force_escape }}
			{% endif %}
		</td>
		<td valign="top" style="padding: 8px; line-height: 18px; text-align: left; vertical-align: top; border-top: 1px solid #dddddd;">
			{% if ans.answer_text|is_list %}
				{% for v in ans.answer_text %}
					{{ v }}{% if not forloop.last %}<br/>{% endif %}
				{% endfor %}
			{% else %}
				{{ ans.answer_text }}
			{% endif %}
		</td>
	</tr>
	{% endif %}
{% endfor %}
</table>

{% endblock %}
