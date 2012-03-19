

<div class="control-group">
    <label class="control-label" for="new-link">{_ Type the title of the page you want to connect to.  Click “Make a new connection” when the page does not yet exist. _}</label>
    <div class="controls">
        {% button id="new-link" text=_"Make a new connection" class="btn"
                action={dialog_close} action={dialog_new_rsc redirect=false subject_id=subject_id predicate=predicate cat=predicate_cat edge_template=edge_template} %}
    </div>
</div>

<div class="control-group">
    <label class="control-label" for="{{#input}}">{_ or use the autocompleter to search the site. _}</label>
    <div class="controls">

        <div class="form-item autocomplete-wrapper clear">
	    <input id="{{#input}}" type="text" value="" class="autocompleter span8 do_autofocus" />
	    <ul id="{{#suggestions}}" class="suggestions-list"></ul>
        </div>
    </div>
</div>

{% wire id=#input
	type="keyup" 
	action={typeselect
				target=#suggestions 
				action_with_id={with_args action={link subject_id=subject_id predicate=predicate element_id=element_id edge_template=edge_template} arg={object_id select_id}}
				action={dialog_close}
				action=action
				cat=m.predicate.object_category[predicate]
			}
%}

{% with m.rsc[("admin_action_link_defaults_"|append:predicate)].id as defaults_id %}
{% if defaults_id %}
<p>
    {_ Alternatively, choose an item from this predefined list: _}
</p>
<ul class="connections-list">
    {% for did in m.search[{query query_id=defaults_id}] %}
    {% if not did|member:m.rsc[subject_id].o[predicate] %}
    <li>
        {% button text=did.title|default:_"untitled"
           class="btn btn-mini"
           action={link subject_id=subject_id predicate=predicate element_id=element_id edge_template=edge_template object_id=did} 
           action={dialog_close}
           action=action
        %}
    </li>
    {% endif %}
    {% endfor %}
</ul>

{% endif %}
{% endwith %}
