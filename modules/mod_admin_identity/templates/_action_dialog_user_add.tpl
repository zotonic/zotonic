
{% wire id=#form type="submit" postback={user_add on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <h3>{_ Name and e-mail address _}</h3>
    <p>
	{_ Give the name and the e-mail address of the new user. A <em>person</em> page will be created for this user. _}
    </p>

    <div class="control-group">
	<label class="control-label" for="{{ #name_first }}">{_ First _}</label>
        <div class="controls">
	    <input type="text" id="{{ #name_first }}" name="name_first" value="" class="do_autofocus" />
	    {% validate id=#name_first name="name_first" type={presence} %}
	</div>
    </div>

    <div class="control-group">
	<label class="control-label" for="{{ #name_surname_prefix }}">{_ Sur. prefix _}</label>
        <div class="controls">
	    {# below we do not use name_surename_prefix because it confuses the autofill of browsers #}
	    <input type="text" id="{{ #name_surname_prefix }}" name="surprefix" value="" style="width: 50px"/>
	</div>
    </div>

    <div class="control-group">
	<label class="control-label" for="{{ #name_surname }}">{_ Surname _}</label>
        <div class="controls">
	    <input type="text" id="{{ #name_surname }}" name="name_surname" value="" />
	    {% validate id=#name_surname name="name_surname" type={presence} %}
	</div>
    </div>

    <div class="control-group">
	<label class="control-label" for="{{ #email }}">{_ E-mail _}</label>
        <div class="controls">
	    <input type="text" id="{{ #email }}" name="email" value="" />
	    {% validate id=#email name="email" type={presence} type={email} %}
	</div>
    </div>

    <hr />

    <h3>{_ Username and password _}</h3>
    <p>
	{_ Give an unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them. _}
    </p>

	
    <div class="control-group">
	<label class="control-label" for="new_username">{_ Username _}</label>
        <div class="controls">
	    <input type="text" id="new_username" name="new_username" value="" />
	    {% validate id="new_username" wait=400 type={presence} type={username_unique} %}
	</div>
    </div>

    <div class="control-group">
	<label class="control-label" for="new_password">{_ Password _}</label>
        <div class="controls">
	    <input type="text" id="new_password" name="new_password" value="" />
	    {% validate id="new_password" type={presence} %}
	</div>
    </div>

    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" %}
	<button class="btn btn-primary" type="submit">{_ Add user _}</button>
    </div>
</form>

