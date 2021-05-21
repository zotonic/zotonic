
{% with #form as form %}

{% wire id=form type="submit" postback={user_add on_success=on_success} delegate=delegate %}
<form id="{{ form }}" method="POST" action="postback" class="form">

    <h4>{_ Name and e-mail address _}</h4>
    <p>
	    {_ Give the name and the e-mail address of the new user. A <em>person</em> page will be created for this user. _}
    </p>

    <div class="row">
        <div class="col-md-4">
            <div class="form-group label-floating">
                <input type="text" id="{{ #name_first }}" name="name_first" value="" class="form-control" tabindex="1" autofocus required placeholder="{_ First _}">
                <label class="control-label" for="{{ #name_first }}">{_ First _}</label>
                {% validate id=#name_first name="name_first" type={presence} %}
           </div>
        </div>
        <div class="col-md-2">
            <div class="form-group label-floating">
                <input class="form-control" type="text" id="{{ #name_surname_prefix }}" name="name_surname_prefix" value="" tabindex="2" autocomplete="additional-name" placeholder="{_ Sur. prefix _}">
                <label class="control-label" for="{{ #name_surname_prefix }}">{_ Sur. prefix _}</label>
            </div>
        </div>
        <div class="col-md-6">
            <div class="form-group label-floating">
                <input class="form-control" type="text" id="{{ #name_surname }}" name="name_surname" value="" tabindex="3" required placeholder="{_ Surname _}">
                {% validate id=#name_surname name="name_surname" type={presence} %}
                <label class="control-label" for="{{ #name_surname }}">{_ Surname _}</label>
            </div>
        </div>
    </div>

    <div class="form-group label-floating">
        <input class="form-control" type="email" id="{{ #email }}" name="email" value="" tabindex="4" required placeholder="{_ E-mail _}">
        {% validate id=#email name="email" type={presence} type={email} %}
        <label class="control-label" for="{{ #email }}">{_ E-mail _}</label>
    </div>

    {% with m.rsc[m.admin_identity.new_user_category].id as cat_id %}
    {% with m.rsc[m.admin_identity.new_user_content_group].id as cg_id %}
        {% block category %}
            <div class="form-group">
        	    <label class="control-label" for="{{ #category }}">{_ Category _}</label>
                <select class="form-control" id="{{ #category }}" name="category" required>
                    {% for category in m.category.person.tree_flat %}
                        {% if m.acl.is_allowed.insert[category.id] %}
                            <option value="{{ category.id.name }}" {% if category.id == cat_id %}selected{% endif %}>
                                {{ category.indent }} {{ category.id.title }}
                            </option>
                        {% endif %}
                    {% endfor %}
                </select>
    	        {% validate id=#category name="category" type={presence} %}
            </div>
        {% endblock %}
    {% endwith %}
    {% endwith %}

    {% block user_extra %}
    {% endblock %}

    <h4>{_ Username and password _}</h4>

    {% include "_identity_password.tpl" %}

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Add user _}</button>
    </div>

</form>

{% endwith %}


