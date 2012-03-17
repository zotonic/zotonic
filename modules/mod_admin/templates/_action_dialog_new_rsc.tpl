{% wire id=#form type="submit" 
	postback={new_page subject_id=subject_id predicate=predicate redirect=redirect edge_template=edge_template actions=actions }
	delegate=delegate 
%}
<p>{_ Please fill in the title _} {% if not nocatselect %}{_ and the category of the new page._}{% else %}{_ of the new _} {{ catname }}.{% endif %} </p>

<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <fieldset>
	<div class="control-group">
	    <label class="control-label" for="new_rsc_title">{_ Page title _}</label>
            <div class="controls">
	        <input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" class="span4" />
		{% validate id="new_rsc_title" type={presence} %}
	    </div>
        </div>

	<div class="control-group">
	    <label class="control-label" for="{{ #category }}">{_ Category _}</label>
            <div class="controls">
	        {% if cat and nocatselect %}
	        <strong>{{ m.rsc[cat].title }}</strong>
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
        
	<div class="control-group">
	    <label class="control-label" for="{{ #published }}">{_ Published _}</label>
            <div class="controls">
		<label class="checkbox">
                    <input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if subject_id %}checked="checked"{% endif %} />
                </label>
	    </div>
	</div>
    </fieldset>

    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" %}
	<button class="btn btn-primary" type="submit">{_ Make _} {{ catname }}</button>
    </div>

</form>

