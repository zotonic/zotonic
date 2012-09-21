<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-find">
	<p>{_ Find an existing page to connect _}</p>

	<form id="dialog-connect-find">
		<input name="find_text" type="text" value="" placeholder="{_ Type text to search _}" class="do_autofocus span8" />
		<select name="find_category">
			{% if predicate %}
				<option value="p:{{ predicate }}">{_ Valid for: _} {{ predicate.title }}</option>
			{% endif %}
			<option value="">{_ Any category _}</option>
			<option value="" disabled></option>
		    {% for cat_id, level, indent, name in m.category.all_flat %}
		    {% if m.acl.insert[name|as_atom] %}
		    <option value="{{cat_id}}" {% ifequal cat_id cat %}selected="selected" {% endifequal %}>
			{{ indent }}{{ m.rsc[cat_id].title|default:name }}
		    </option>
		    {% endif %}
		    {% endfor %}
		</select>
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
