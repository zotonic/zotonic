{% if id == 1 %}
    <p>
        {_ The password of the admin user cannot be changed in the database. Please edit your site's configuration file at _} <strong>priv/sites/{{ m.site.host }}/config</strong> {_ if you want to change the admin password. _}
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
            {% if username and id != m.acl.user %}
                {% button class="btn btn-danger pull-left"
                        postback={delete_username id=id on_delete={dialog_close} on_delete=on_delete}
                        delegate=delegate
                        text=_"Delete"
                        tag="a"
                %}
            {% endif %}
	        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	        <button class="btn btn-primary" type="submit">{_ Save _}</button>
        </div>
    </form>
{% endif %}
