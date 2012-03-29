{% with m.config.i18n.language_list.list as languages %}
{% with m.rsc[id].language as r_lang %}

{% wire id=#form type="submit" 
	postback={rsc_edit_basics id=id edge_id=edge_id update_element=update_element template=template actions=actions}
	delegate=delegate 
%}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <div class="tabbable">
	<ul class="nav nav-pills">
	    <li class="active"><a data-toggle="tab" href="#{{ #main }}">{_ Main _}</a></li>
	    <li><a data-toggle="tab" href="#{{ #acl }}">{_ Access control _}</a></li>
	</ul>

        <div class="tab-content">
	<div class="tab-pane active" id="{{ #main }}">
	    {% for code, lang in languages %}
            {% if lang.is_enabled %}
                {% if code|member:r_lang or (not r_lang and z_language == code) %}
            	    <input type="hidden" id="{{ #language.code }}" name="language" value="{{ code }}" /> 
        	    {% endif %}
            {% endif %}
        {% endfor %}
        
	    {% all catinclude "_admin_edit_basics.tpl" id in_dialog is_editable=id.is_editable languages=languages %}
			
	    {% if id.is_a.meta %}
	    <div class="control-group">
		<label class="control-label" for="{{ #unique }}">{_ Unique name _}</label>
                <div class="controls">
		    <input type="text" id="{{ #unique }}" name="name" value="{{ id.name }}" />
		    {% validate id=#unique name="name" type={presence} %}
                </div>
	    </div>
	    {% else %}
	    <div class="control-group">
		<label class="control-label" for="category_id">{_ Category _}</label>
                <div class="controls">
                    {% include "_admin_category_dropdown.tpl" id=id %}
                </div>
	    </div>
	    {% endif %}
            
	    <div class="control-group">
	        <label class="control-label" for="{{ #published }}">{_ Published _}</label>
                <div class="controls">
		    <label class="checkbox">
                        <input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %} />
                    </label>
	        </div>
	    </div>

	</div>
        
	<div class="tab-pane" id="{{ #acl }}">
	    {% include "_admin_edit_visible_for.tpl" id=id %}
	</div>
        </div>
    </div>
    
    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" %}
	{% button class="btn" text=_"Visit full editpage" action={redirect dispatch="admin_edit_rsc" id=id} %}
	{% button class="btn btn-primary" text=_"Save" %}
    </div>
</form>

{% endwith %}
{% endwith %}
