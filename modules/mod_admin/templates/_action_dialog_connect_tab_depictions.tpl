<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-depiction">
	<p>{_ Select a connected image _}</p>

	<div id="dialog_connect_depictions" class="connect-results thumbnails">
		<div class="row">
            {% for id in m.rsc[subject_id].o.depiction %}
                {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id %}
            {% endfor %}
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
    $("#dialog_connect_depictions").on('click', '.thumbnail', function() {
        z_event('dialog_connect_depiction', { 
            select_id: $(this).data('id')
        });
    });
{% endjavascript %}
