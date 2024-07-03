{% if id == 1 %}
    <p>
        {_ The password of the admin user cannot be changed in the database. Please edit your site’s configuration file in <tt>priv/zotonic_site.config</tt> if you want to change the admin password. _}
    </p>
{% else %}
    <p>
	    {_ Enter a unique username and password. Usernames and passwords are case sensitive, so be careful when entering them. _}
	    {% if username and m.acl.user != id and id != 1 %}
	        {_ Click “delete” to remove any existing username/password from the person; this person will no longer be a user. _}
	    {% endif %}
    </p>

    {% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
    <form id="{{ #form }}" method="POST" action="postback" class="form">
        <input type="hidden" name="id" value="{{ id }}" />

        {% include "_identity_password.tpl" %}

        <div class="modal-footer">
            {% if username and id != m.acl.user and id != 1 %}
                {% button class="btn btn-danger pull-left" text=_"Delete Username"
                          action={dialog_delete_username id=id}
                %}
            {% endif %}
	        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	        <button class="btn btn-primary" type="submit">{_ Save _}</button>
        </div>
    </form>
{% endif %}
