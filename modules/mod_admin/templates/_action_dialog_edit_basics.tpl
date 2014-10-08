{% with m.config.i18n.language_list.list as languages %}
    {% with m.rsc[id].language as r_lang %}

        {% wire id=#form type="submit" 
	        postback={rsc_edit_basics id=id edge_id=edge_id update_element=update_element
				template=template actions=actions callback=callback is_update=is_update}
	        delegate=delegate 
        %}
        <form id="{{ #form }}" method="POST" action="postback" class="form">

            <div class="tabbable">
                {% block tabbar %}
	                <ul class="nav nav-pills">
	                    <li class="active"><a data-toggle="tab" href="#{{ #main }}">{_ Main _}</a></li>
	                    <li><a data-toggle="tab" href="#{{ #acl }}">{_ Access control _}</a></li>
	                </ul>
                {% endblock %}
                
                <div class="tab-content">
                    {% block tab_content %}
	                    <div class="tab-pane active" id="{{ #main }}">

	                        {% catinclude "_admin_edit_basics.tpl" id in_dialog is_editable=id.is_editable languages=languages %}
			                
	                        {% if id.is_a.meta %}
	                            <div class="form-group row">
		                            <label class="control-label col-md-3" for="{{ #unique }}">{_ Unique name _}</label>
                                    <div class="col-md-9">
		                                <input class="form-control" type="text" id="{{ #unique }}" name="name" value="{{ id.name }}" />
		                                {% validate id=#unique name="name" type={presence} %}
                                    </div>
	                            </div>	
	                        {% endif %}
                            
	                        <div class="form-group row">
	                            <label class="control-label col-md-3" for="{{ #published }}">{_ Published _}</label>
                                <div class="col-md-9">
                                    <div class="checkbox">
		                                <label>
                                            <input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %} />
                                        </label>
	                                </div>
                                </div>
	                        </div>
	                    </div>
                    {% endblock %}
                    
                    {% block tab_visible_for %}
	                    <div class="tab-pane" id="{{ #acl }}">
	                        {% include "_admin_edit_visible_for.tpl" id=id %}
	                    </div>
                    {% endblock %}

                    {% block tab_extra %}
                    {% endblock %}
                </div>
            </div>
            
            <div class="modal-footer">

	            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default">{_ Visit full edit page _}</a>
	            {% button class="btn btn-primary" type="submit" text=_"Save" %}
            </div>
        </form>

    {% endwith %}
{% endwith %}
