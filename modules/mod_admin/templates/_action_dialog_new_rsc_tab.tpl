{% wire id=#form type="submit" 
	postback={new_page subject_id=subject_id predicate=predicate redirect=redirect 
			  actions=actions callback=callback}
	delegate=delegate 
%}
<p>{_ Please fill in the title _} {% if not nocatselect %}{_ and the category of the new page._}{% else %}{_ of the new _} {{ m.rsc[cat].title }}.{% endif %} </p>

<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <fieldset>
	<div class="control-group">
	    <label class="control-label" for="new_rsc_title">{_ Page title _}</label>
	    <div class="controls">
		<input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" class="span4 do_autofocus" />
		{% validate id="new_rsc_title" type={presence} %}
	    </div>
	</div>

	<div class="control-group">
	    <label class="control-label" for="{{ #category }}">{_ Category _}</label>
	    <div class="controls">
		{% if cat and nocatselect %}
		<input type="text" readonly value="{{ m.rsc[cat].title }}" />
		<input type="hidden" name="category_id" value="{{ cat }}"/>
		{% else %}
		<select id="{{ #category }}" name="category_id" class="span4">
		    {% for cat_id, level, indent, name in m.category.all_flat %}
		    {% if m.acl.insert[name|as_atom] %}
		    <option value="{{cat_id}}" {% ifequal cat_id cat %}selected="selected" {% endifequal %}>
			{{ indent }}{{ m.rsc[cat_id].title|default:name }}
		    </option>
		    {% endif %}
		    {% endfor %}
		</select>
		{% endif %}
	    </div>
	</div>

    {% if m.rsc[cat].is_a.meta %}
	<div class="control-group">
	    <label class="control-label" for="{{ #published }}">{_ Name _}</label>
	    <div class="controls">
		    <input type="input" id="{{ #name }}" name="name" value="" />
			{% validate id=#name name="name" type={presence} %}
	    </div>
	</div>
    {% endif %}
	
	<div class="control-group">
	    <label class="control-label" for="{{ #published }}">{_ Published _}</label>
	    <div class="controls">
		<label class="checkbox">
		    <input type="checkbox" id="{{ #published }}" name="is_published" value="1" 
				{% if subject_id %}checked="checked"{% endif %} />
		</label>
	    </div>
	</div>
    </fieldset>

    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
	<button class="btn btn-primary" type="submit">{_ Make _} {{ catname }}</button>
    </div>

</form>

