{% wire
    id=#form type="submit"
	postback={new_page
	    subject_id=subject_id
        object_id=object_id
	    predicate=predicate
	    redirect=redirect
	    actions=actions
	    callback=callback
        objects=objects
	}
	delegate=delegate
%}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
{% with #form as form %}

	<div class="form-group row">
	    <label class="control-label col-md-3" for="new_rsc_title">{_ Page title _}</label>
	    <div class="col-md-9">
		    <input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" class="do_autofocus form-control" />
		    {% validate id="new_rsc_title" type={presence} %}
	    </div>
	</div>

	{% block category %}
		<div class="form-group row">
		    <label class="control-label col-md-3" for="{{ #category }}">{_ Category _}</label>
		    <div class="col-md-9">
			    {% if cat and nocatselect %}
				    <input class="form-control" type="text" readonly value="{{ m.rsc[cat].title }}" />
				    <input type="hidden" name="category_id" value="{{ cat }}"/>
			    {% else %}
				    {% block category_select %}
				        <select class="form-control" id="{{ #category }}" name="category_id">
							<option></option>
				            {% for c in m.category.tree_flat %}
				                {% if m.acl.insert[c.id.name|as_atom] %}
				                    <option value="{{c.id}}" {% if c.id == cat %}selected="selected" {% endif %}>
					                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
				                    </option>
				                {% endif %}
				            {% endfor %}
				        </select>
						{% validate id=#category name="category_id" type={presence} %}
				    {% endblock %}
			    {% endif %}
		    </div>
		</div>
	{% endblock %}

	{% all include "_dialog_new_rsc_extra.tpl" %}

    {% if cat.name == 'category' or cat.name == 'predicate' %}
	    <div class="form-group row">
	        <label class="control-label col-md-3" for="{{ #name }}">{_ Name _}</label>
	        <div class="col-md-9">
		        <input class="form-control" type="text" id="{{ #name }}" name="name" value="" />
			    {% validate id=#name name="name" type={presence} %}
	        </div>
	    </div>
    {% endif %}

	<div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #published }}">{_ Published _}</label>
	    <div class="checkbox col-md-9">
		    <label>
		        <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
				    {% if subject_id or m.admin.rsc_dialog_is_published %}checked="checked"{% endif %} />
		    </label>
	    </div>
	</div>

    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	    <button class="btn btn-primary" type="submit">{_ Make _} {{ catname }}</button>
    </div>
{% endwith %}
</form>

