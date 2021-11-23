{% with id.blocks['survey_feedback'] as blk %}
{% if blk and (blk.type /= 'text' or blk.body) %}
    {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
{% else %}
	<h2>{_ Thank you _}</h2>

	<p>{_ Thank you for filling in our survey. _}</p>
{% endif %}
{% endwith %}

{% if is_overlay %}
	<p>
	    <button id="{{ #survey_close }}" class="btn btn-lg btn-primary">{_ Close _}</button>
	    {% wire id=#survey_close
	            action={overlay_close}
	    %}
	</p>
{% endif %}

