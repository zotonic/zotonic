<fieldset class="form">


    {% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
	    <div class="col-md-6">
            <div class="well">
                <h4 style="margin-top: 0">{_ User actions _}</h4>
                <div class="form-group">
                    {% button class="btn btn-default" action={dialog_set_username_password id=id} text=_"Set username / password" %}
                </div>

		        {% if m.acl.is_admin and m.identity[id].is_user and id != m.acl.user %}
                    <div class="form-group">
			            {% button class="btn btn-default" action={confirm text=_"Click OK to log on as this user. You will be redirected to the home page if this user has no rights to access the admin system." postback={switch_user id=id} delegate=`mod_admin_identity`} text=_"Log on as this user" %}
                    </div>
		        {% endif %}

                {% if id /= 1 %}
                    <div>
                        {% button class="btn btn-default" text=_"delete username" action={dialog_delete_username id=id on_success={slide_fade_out target=#tr.id}} %}
                </div>
            {% endif %}
        </div>        
	</div>
{% endif %}

    {% all include "_admin_edit_basics_user_extra.tpl" %}

</fieldset>
