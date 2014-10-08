{% with id.blocks['survey_feedback'] as blk %}
{% if blk and (blk.type /= 'text' or blk.body) %}
    {% include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
{% else %}
	<h2>{_ Thank you _}</h2>

	<p>{_ Thank you for filling in our survey. Be sure to come back for other surveys! _}</p>
{% endif %}
{% endwith %}
