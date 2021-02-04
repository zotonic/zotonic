{% wire id=#form type="submit"
    postback={
        rsc_edit_basics
        id=id
        edge_id=edge_id
        update_element=update_element
		template=template
		actions=actions
		callback=callback
		is_update=is_update
	}
    delegate=delegate
%}
<form id="{{ #form }}" method="POST" action="postback" class="form">
    <div class="tabbable">
        {% block tabbar %}
            <ul class="nav nav-pills">
                <li class="active"><a data-toggle="tab" href="#{{ #main }}">{_ Main _}</a></li>
                <li><a data-toggle="tab" href="#{{ #acl }}">{_ Access control _}</a></li>
                {% block tabbar_extra %}
                {% endblock %}
                </ul>
        {% endblock %}
        <div class="tab-content">
            {% block tab_content %}
                <div class="tab-pane active" id="{{ #main }}">

                    {% catinclude "_admin_edit_basics.tpl" id in_dialog show_header %}

                    {% if id.is_a.meta %}
                        <div class="form-group label-floating">
                            <input class="form-control" type="text" id="{{ #unique }}" name="name" value="{{ id.name }}" placeholder="{_ Unique name _}">
                            {% if id.is_a.category or id.is_a.predicate %}
                                {% validate id=#unique
                                            name="name"
                                            type={presence}
                                            type={format pattern="^[A-Za-z0-9_]*$"}
                                %}
                            {% else %}
                                {% validate id=#unique
                                            name="name"
                                            type={format pattern="^[A-Za-z0-9_]*$"}
                                %}
                            {% endif %}
                            <label class="control-label" for="{{ #unique }}">{_ Unique name _}</label>
                        </div>
                    {% endif %}

                    <div class="form-group">
                        <label class="checkbox">
                            <input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}>
                            {_ Published _}
                        </label>
                    </div>
                </div>
            {% endblock %}

            {% block tab_visible_for %}
                <div class="tab-pane" id="{{ #acl }}">
                    {% optional include "_admin_edit_visible_for.tpl" id=id %}
                </div>
            {% endblock %}

            {% block tab_extra %}
            {% endblock %}
        </div>
    </div>

    {% block modal_footer %}
        <div class="modal-footer">
            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
            {% if m.acl.use.mod_admin or m.acl.use.mod_admin_frontend %}
                <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default">{_ Visit full edit page _}</a>
            {% endif %}
            {% button class="btn btn-primary" type="submit" text=_"Save" %}
        </div>
    {% endblock %}
</form>
