<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-depiction">
	<p>{_ Select a connected image _}</p>

	<div id="dialog_connect_depictions" class="connect-results thumbnails">

		<div class="row">
            {% with m.rsc[subject_id].o.depiction
                 ++ m.rsc[subject_id].o.hasdocument
               as depictions
            %}
            {% with m.rsc[subject_id].o.refers as refers %}
            {% with depictions ++ (refers -- depictions) as deps %}
                {% if deps %}
                    {% for id in deps %}
                        {% if id.medium %}
                            {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id %}
                        {% endif %}
                    {% endfor %}
                {% else %}
                    <div class="col-lg-4 col-md-4">
                        {_ No connected images. _}
                    </div>
                {% endif %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
		</div>
	</div>
</div>
{% wire name="dialog_connect_depiction"
    action={postback
        delegate=delegate|default:"mod_admin"
        postback={admin_connect_select
            intent=intent
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
