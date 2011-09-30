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
					<label>{_ Category _}</label>
                    {% include "_admin_category_dropdown.tpl" id=id %}
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<label>{_ Published? _}</label>
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
