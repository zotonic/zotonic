{% with m.oauth2_consumer.consumers[app_id] as app %}
{% wire id=#new
        type="submit"
        postback={oauth2_consumer_token_new app_id=app.id}
        delegate=`mod_oauth2`
%}
<form id="{{ #new }}" action="postback">
    <p>
        {% trans "Add a new token for accessing data on <b>{domain}</b>." domain=app.domain|escape %}
    </p>

    <div class="form-group">
        <p class="help-block">
            {_ Select the user for this token. _} {_ This user will be able to use the token. _}
            <br>
            {_ If the user has already a token, then that token will be replaced with the new token. _}
        </p>
        <div class="block-page">
            <div class="well" id="{{ #wrap }}">
                {% include "_rsc_item_select.tpl" id=m.acl.user %}
            </div>
            <button class="btn btn-default page-connect" id="{{ #connect }}" type="button">
                {_ Select a user _}
            </button>
            {% wire id=#connect
                    action={dialog_open
                        title=_"Find user"
                        template="_action_dialog_connect.tpl"
                        intent="select"
                        category=`person`
                        content_group="any"
                        tabs_enabled=[ "find", "new" ]
                        center=false
                        autoclose
                        width="large"
                        level=2
                        actions=[
                            {update
                                target=#wrap
                                template="_rsc_item_select.tpl"
                            },
                            {set_value
                                target=#user_id
                                value_arg=`select_id`
                            }
                        ]

                    }
            %}
            <input type="hidden" name="user_id" id="{{ #user_id }}" value="{{ m.acl.user }}" required>
            {% validate id=#user_id name="user_id" type={presence} %}
        </div>
    </div>

    {% if app.grant_type == 'client_credentials' and app.has_credentials %}
        <hr>

        <div class="form-group">
            <p class="help-block">
                {% trans "As the application is configured to use {client}, you can fetch an access token from {domain}." domain=app.domain|escape client="Client Credentials" %}
            </p>

            <p class="text-center">
                <button class="btn btn-primary" type="submit" name="fetch">
                    {_ Fetch Token _}
                </button>
            </p>
        </div>
    {% endif %}

    <hr>

    <p class="help-block">
        {% trans "Manually add a token, as copied from {domain}." domain=app.domain|escape %}
    </p>

    <div class="form-group">
        <div class="label-floating">
            <input type="text" value="" class="form-control do_autofocus" name="token" placeholder="{_ Token _}">
            <label class="control-label">{_ Token _}</label>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" type="button" text=_"Cancel" action={dialog_close} %}
        {% button class="btn btn-primary" type="submit" text=_"Add Token" %}
    </div>
</form>
{% endwith %}
