{% if m.oauth2.apps[app_id] as app %}
    {% wire id=#new
            type="submit"
            postback={oauth2_app_update app_id=app.id}
            delegate=`mod_oauth2`
    %}

    <form id="{{ #new }}" action="postback">
        <p>
            {_ Edit the description of the App. _}
        </p>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_enabled" {% if app.is_enabled %}checked{% endif %}> {_ Enabled _}
            </label>
        </div>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #description }}" type="text" class="form-control" autofocus name="description" value="{{ app.description|escape }}" required placeholder="{_ Description _}">
                <label class="control-label" for="description">{_ Description _}</label>
                {% validate id=#description name="description" type={presence} %}
            </div>
        </div>

        <div class="well">
            <div class="row">
                <div class="col-sm-6">
                    <p>
                        <label>{_ App ID _}</label><br>
                        <tt>{{ app.id|escape }}</tt>
                    </p>
                </div>
                <div class="col-sm-6">
                    <p>
                        <label>{_ App Secret _}</label><br>
                        <tt>{{ app.app_secret|escape }}</tt>
                    </p>
                </div>
            </div>
        </div>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_allow_auth" value="1" id="{{ #auth }}"
                     {% if app.is_allow_auth %}checked{% endif %}
               >
                {_ Allow OAuth2 authorization with users on this site _}
            </label>
            {% javascript %}
                $('#{{ #auth }}').on('input', function() {
                    if ($(this).is(":checked")) {
                        $('#{{ #auth_options }}').fadeIn();
                    } else {
                        $('#{{ #auth_options }}').fadeOut();
                    }
                });
            {% endjavascript %}        </div>

        <div id="{{ #auth_options }}" class="well"
            {% if not app.is_allow_auth %}style="display: none"{% endif %}
        >
            <div class="form-group">
                <div class="label-floating">
                    <textarea id="{{ #redirect_urls }}" class="form-control" name="redirect_urls" placeholder="{_ Valid redirect URLs, one per line _}">{{ app.redirect_urls|escape }}</textarea>
                    <label class="control-label" for="redirect_urls">{_ Valid redirect URLs, one per line _}</label>
                    <p class="help-block">
                        {_ Give the redirect URLs that are valid for the website performing the OAuth2 authorization. _}
                        {_ These must be complete URLs, but without the query (?..) or hash (#...) parts. _}<br>
                        {_ For Zotonic sites this you can enter the domain name(s) of the website. _}
                    </p>
                </div>
            </div>
        </div>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_allow_client_credentials" value="1" id="{{ #cc }}"
                    {% if app.is_allow_client_credentials %}checked{% endif %}
                >
                {_ Allow fetching tokens using Client Credentials _}
            </label>
            {% javascript %}
                $('#{{ #cc }}').on('input', function() {
                    if ($(this).is(":checked")) {
                        $('#{{ #cc_options }}').fadeIn();
                    } else {
                        $('#{{ #cc_options }}').fadeOut();
                    }
                });
            {% endjavascript %}
        </div>

        <div id="{{ #cc_options }}" class="well"
            {% if not app.is_allow_client_credentials %}style="display: none"{% endif %}
        >
            <div class="form-group">
                <label class="control-label">{_ Client Credentials token expiration _}</label>
                <p class="help-block">
                    {_ Tokens fetched using Client Credentials will expire after this period. _}
                    {_ The client must extend the token before it expires. _}
                </p>
                <select name="client_credentials_expires" class="form-control" style="width: auto">
                    {% for exp, t in [
                            [ 24 * 3600, _"Day" ],
                            [ 7 * 24 * 3600, _"Week" ],
                            [ 31 * 24 * 3600, _"Month" ],
                            [ 366 * 24 * 3600, _"Year" ],
                            [ 0, _"Forever" ]
                       ]
                    %}
                        <option value="{{ exp }}" {% if app.client_credentials_expires == exp %}selected{% endif %}>
                            {{ t }}
                        </option>
                    {% endfor %}
                </select>
            </div>

            <div class="form-group">
                <label class="control-label">{_ Client Credentials user _}</label>
                <p class="help-block">{_ Select the user associated with Client Credential tokens. _} {_ It is good practice to have users with limited access permissions for access tokens. _}</p>
                <div class="block-page">
                    <div class="well" id="{{ #wrap }}">
                        {% if app.client_credentials_user_id %}
                            {% include "_rsc_item_select.tpl" id=app.client_credentials_user_id %}
                        {% else %}
                            <span class="text-muted">{_ No user selected. _}</span>
                        {% endif %}
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
                    <input type="hidden" name="client_credentials_user_id" id="{{ #user_id }}" value="{{ app.client_credentials_user_id }}">
                </div>
            </div>

            <div class="form-group">
                <label class="control-label">{_ Access permission _}</label>
                <p class="help-block">{_ <b>Important:</b> write access is only disabled if the used ACL module and models support this. _}</p>

                <label class="radio">
                    <input type="radio" name="is_client_credentials_read_only" value="1"
                        {% if app.is_client_credentials_read_only %}checked{% endif %}>
                    {_ Read only access _}
                </label>
                <label class="radio">
                    <input type="radio" name="is_client_credentials_read_only" value="0"
                        {% if not app.is_client_credentials_read_only %}checked{% endif %}>
                    {_ Read &amp; write access _}
                </label>
            </div>
        </div>

        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
            {% button class="btn btn-primary" type="submit" text=_"Update App" %}

            {% button class="btn btn-danger pull-left" type="submit" text=_"Delete App"
                action={confirm
                    text=_"Are you sure you want to delete this App?"
                    is_danger
                    ok=_"Delete App"
                    postback={oauth2_app_delete app_id=app.id}
                    delegate=`mod_oauth2`
                }
            %}
        </div>
    </form>
{% else %}
    <p class="alert alert-danger">
        {_ App not found, or no view permission. _}
    </p>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
    </div>
{% endif %}
