
{% wire id=#form type="submit" postback={user_add on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">

    <h4>{_ Name and e-mail address _}</h4>
    <p>
	    {_ Give the name and the e-mail address of the new user. A <em>person</em> page will be created for this user. _}
    </p>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #name_first }}">{_ First _}</label>
        <div class="col-md-6">
	        <input type="text" id="{{ #name_first }}" name="name_first" value="" class="form-control" tabindex="1" autofocus />
	        {% validate id=#name_first name="name_first" type={presence} %}
	    </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #name_surname_prefix }}">{_ Sur. prefix _}</label>
        <div class="col-md-9">
	        <input class="form-control" type="text" id="{{ #name_surname_prefix }}" name="name_surname_prefix" value="" style="width: 50px" tabindex="2" autocomplete="additional-name" />
	    </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #name_surname }}">{_ Surname _}</label>
        <div class="col-md-9">
	        <input class="form-control" type="text" id="{{ #name_surname }}" name="name_surname" value="" tabindex="3" />
	        {% validate id=#name_surname name="name_surname" type={presence} %}
	    </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #email }}">{_ E-mail _}</label>
        <div class="col-md-9">
	        <input class="form-control" type="email" id="{{ #email }}" name="email" value="" tabindex="4" />
	        {% validate id=#email name="email" type={presence} type={email} %}
	    </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #category }}">{_ Category _}</label>
        <div class="col-md-9">
            <select class="form-control" id="{{ #category }}" name="category">
                {% with m.admin_identity.new_user_category as default_category %}
                    {% for category in m.category.person.tree_flat %}
                        {% if m.acl.is_allowed.insert[category.id] %}
                            <option value="{{ category.id.name }}" {% if category.id.name == default_category %}selected{% endif %}>
                                {{ category.indent }} {{ category.id.title }}
                            </option>
                        {% endif %}
                    {% endfor %}
                {% endwith %}
            </select>
	        {% validate id=#category name="category" type={presence} %}
	    </div>
    </div>

    <hr />

    <h4>{_ Username and password _}</h4>

    {% include "_identity_password.tpl" %}

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Add user _}</button>
    </div>

</form>

