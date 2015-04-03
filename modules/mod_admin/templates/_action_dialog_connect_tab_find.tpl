<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-find">
	<p>{_ Find an existing page to connect _}</p>

	<form id="dialog-connect-find" class="row form form-horizontal">
        <div class="col-md-8">
		    <input name="find_text" type="text" value="" placeholder="{_ Type text to search _}" class="do_autofocus form-control" />
        </div>

        <div class="col-md-4">
            
		    {% block category_select %}
		        <select class="form-control" name="find_category">
			        {% if predicate %}
				        <option value="p:{{ predicate }}">{_ Valid for: _} {{ predicate.title }}</option>
			        {% endif %}
			        <option value="">{_ Any category _}</option>
			        <option value="" disabled></option>
		            {% for c in m.category.tree_flat %}
			            <option value="{{ c.id }}" {% if c.id == cat %}selected="selected" {% endif %}>
					        {{ c.indent }}{{ c.id.title|default:c.id.name }}
			            </option>
		            {% endfor %}
		        </select>
	        {% endblock %}
        </div>
	</form>

	<div id="dialog-connect-found" class="do_feedback"
		data-feedback="trigger: 'dialog-connect-find', delegate: 'mod_admin'">
	</div>
</div>

{% javascript %}

    $('#dialog-connect-find').change();

    $("#dialog-connect-found").on('click', '.thumbnail', function() {
        z_notify("admin-connect-select", { 
            z_delegate: "mod_admin", 
            select_id: $(this).data('id'),
            predicate: '{{ predicate }}',
            subject_id: '{{ subject_id }}',
            callback: '{{ callback }}',
            language: '{{ language }}'
        });
    });
{% endjavascript %}
