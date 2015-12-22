<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-find">
	<form id="dialog-connect-find" class="row form form-horizontal">
		<input type="hidden" name="subject_id" value="{{ subject_id }}" />
		<input type="hidden" name="predicate" value="{{ predicate|default:'' }}" />

        <div class="col-md-8">
		    <input name="find_text" type="text" value="{{ text|default:'' }}" placeholder="{_ Type text to search _}" class="do_autofocus form-control" />
        </div>

        <div class="col-md-4">
		    {% block category_select %}
		        <select class="form-control" name="find_category">
			        {% if predicate %}
				        <option value="p:{{ predicate }}" selected="selected">{_ Valid for: _} {{ predicate.title }}</option>
			        {% endif %}
			        <option value="">{_ Any category _}</option>
			        <option value="" disabled></option>
                    {% if cat.is_a.meta %}
                        {% for c in m.category.tree_flat_meta %}
                            <option value="{{ c.id }}" {% if c.id == cat and not predicate %}selected="selected"{% endif %}>
                                {{ c.indent }}{{ c.id.title|default:c.id.name }}
                            </option>
                        {% endfor %}
                    {% else %}
    		            {% for c in m.category.tree_flat %}
    			            <option value="{{ c.id }}" {% if c.id == cat and not predicate %}selected="selected"{% endif %}>
    					        {{ c.indent }}{{ c.id.title|default:c.id.name }}
    			            </option>
    		            {% endfor %}
                    {% endif %}
		        </select>
	        {% endblock %}
        </div>
	</form>

	<div id="dialog-connect-found" class="do_feedback"
		data-feedback="trigger: 'dialog-connect-find', delegate: 'mod_admin'">
	</div>
</div>
{% wire name="dialog_connect_find"
    action={postback
        delegate=delegate|default:"mod_admin"
        postback={admin_connect_select
            id=id
            subject_id=subject_id
            predicate=predicate
            callback=callback
            language=language
            action=action
            actions=actions
        }
    }
%}
{% javascript %}
    $('#dialog-connect-find').submit(function() { return false; });
    $('#dialog-connect-find').change();
    $("#dialog-connect-found").on('click', '.thumbnail', function(e) {
    	e.preventDefault();
        z_event('dialog_connect_find', { 
            select_id: $(this).data('id')
        });
    });
{% endjavascript %}
