{% if id == 1 %}
    <p>
        {_ The password of the admin user cannot be changed in the database. Please edit your site's configuration file at _} <strong>priv/sites/{{ m.site.host }}/config</strong> {_ if you want to change the admin password. _}
    </p>

{% else %}
    {% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
    <form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
        <input type="hidden" name="id" value="{{ id }}" />

        {% include "_identity_password.tpl" %}

        <div class="modal-footer">
	        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	        <button class="btn btn-primary" type="submit">{_ Save _}</button>

            {% if id /= 1 %}
                {% button class="btn btn-danger pull-left" text=_"Delete Username" action={dialog_delete_username id=id} %}
            {% endif %}
        </div>
    </form>
{% endif %}
