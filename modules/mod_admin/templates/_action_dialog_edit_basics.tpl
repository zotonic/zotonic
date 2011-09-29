{% wire id=#form type="submit" 
	postback={rsc_edit_basics id=id edge_id=edge_id update_element=update_element template=template}
	delegate=delegate 
%}
<form id="{{ #form }}" method="POST" action="postback">

	{% tabs id=#tabs %}
	<div id="{{ #tabs }}">
		<ul class="clearfix">
			<li><a href="#{{ #main }}">{_ Main _}</a></li>
			<li><a href="#{{ #acl }}">{_ Access control _}</a></li>
		</ul>

		<div id="{{ #main }}">
			{% if m.modules.info.mod_translation.enabled %}
			{% catinclude "_admin_edit_basics_form.tpl" id in_dialog lang_code=z_language lang_code_with_dollar=["$", z_language]|join lang_code_with_brackets=["(", z_language, ")"]|join %}
			{% else %}
			{% catinclude "_admin_edit_basics_form.tpl" id in_dialog %}
			{% endif %}
			
			{% if id.is_a.meta %}
				<div class="form-item">
					<label>{_ Unique name _}</label>
					<input type="text" id="{{ #unique }}" name="name" value="{{ id.name }}" />
					{% validate id=#unique name="name" type={presence} %}
				</div>
			{% else %}
				<div class="form-item">
					{% with id.category_id as r_cat %}
					<label>{_ Category _}</label>
					<select id="category_id" name="category_id">
						{% for cat_id, level, indent, name in m.category.all_flat %}
						{% if m.acl.insert[name|as_atom] %}
							<option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
							{{ indent }}{{ m.rsc[cat_id].title|default:name }}
							</option>
						{% endif %}
						{% endfor %}
					</select>
					{% endwith %}
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<label>&nbsp;</label>
				<input type="checkbox" class="do_fieldreplace" id="is_published" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
				<label for="is_published" class="left">{_ Published _}</label>
			</div>
		</div>

		<div id="{{ #acl }}">
			<div class="admin-form clearfix">
				{% include "_admin_edit_visible_for.tpl" id=id %}
			</div>
		</div>
	</div>

	<div class="form-item clearfix">
		{% button text=_"Save" %}
		{% button text=_"Visit full editpage" action={redirect dispatch="admin_edit_rsc" id=id} %}
		{% button action={dialog_close} text=_"Cancel" %}
	</div>
</form>
