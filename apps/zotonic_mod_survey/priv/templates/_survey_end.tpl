{% with id.blocks['survey_feedback'] as blk %}
{% if blk and (blk.type /= 'text' or blk.body) %}
    {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
{% else %}
	<h2>{_ Thank you _}</h2>

	<p>{_ Thank you for filling in our survey. _}</p>
{% endif %}
{% endwith %}

{% if viewer == 'overlay' %}
	<p>
	    <button id="{{ #survey_close }}" class="btn btn-lg btn-primary">{_ Close _}</button>
	    {% wire id=#survey_close
	            action={overlay_close}
	    %}
	</p>
{% elseif viewer == 'dialog' %}
	<p>
	    <button id="{{ #survey_close }}" class="btn btn-lg btn-primary">{_ Close _}</button>
	    {% wire id=#survey_close
	            action={dialog_close}
	    %}
	</p>
{% else %}
    <p>
        <a href="#" id="{{ #close }}" style="display:none" class="btn btn-primary">{_ Close _}</a>
        <a href="#" id="{{ #back_history }}" style="display:none" class="btn btn-primary">{_ Back _}</a>
    </p>
    {% javascript %}
        if (window.opener) {
            $('#{{ #close }}')
                .show()
                .click(function() {
                    try {
                        window.opener.z_reload();
                    } catch (_E) { };
                    window.close();
                    return false;
                });
        } else if (window.history.length > 0) {
            $('#{{ #back_history }}')
                .show()
                .click(function() {
                    window.history.go(-1);
                    return false;
                });
        }
    {% endjavascript %}
{% endif %}

