{% extends "_admin_edit_blocks.tpl" %}

{% block blocks_before %}
	{% include "_admin_survey_edit_feedback.tpl" %}
	{% include "_admin_survey_edit_email_text.tpl" %}
	<hr/>
	<h4>{_ Questions _}</h4>
	<p>{_ Add questions by adding blocks with the menu. _}</p>
{% endblock %}

{% block blocks %}
    {% for blk in id.blocks %}
    	{% if blk.name /= 'survey_feedback' %}
	        {% include "_admin_edit_block_li.tpl" %}
	    {% endif %}
    {% endfor %}
{% endblock %}

{% block blocks_after %}
	<h4>{_ End of questions _}</h4>
	<hr/>
{% endblock %}
