<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-depiction">
	<p>{_ Select a connected image _}</p>

	<div id="dialog_connect_depictions" class="connect-results thumbnails">
	    
		<div class="row">
		    {% with m.rsc[subject_id].o.depiction as depictions %}
		        {% if depictions %}
                    {% for id in depictions %}
                        {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id %}
                    {% endfor %}
                {% else %}
                    <div class="col-lg-4 col-md-4">
                        {_ No connected images. _}
                    </div>
                {% endif %}
            {% endwith %}
		</div>
	</div>
</div>
{% wire name="dialog_connect_depiction"
    action={postback
        delegate=delegate|default:"mod_admin"
        postback={admin_connect_select
            id=id
            subject_id=subject_id
            predicate=predicate
            callback=callback
            language=language
            action=action
        }
    }
%}
{% javascript %}
    $("#dialog_connect_depictions").on('click', '.thumbnail', function(e) {
        e.preventDefault();
        z_event('dialog_connect_depiction', { 
            select_id: $(this).data('id')
        });
    });
{% endjavascript %}
