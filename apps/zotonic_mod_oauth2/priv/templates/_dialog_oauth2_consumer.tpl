{% if m.oauth2_consumer.consumers[app_id] as app %}
    {% wire id=#new
            type="submit"
            postback={oauth2_consumer_update app_id=app.id}
            delegate=`mod_oauth2`
    %}
    <form id="{{ #new }}" action="postback">
        <p>
            {_ Update consumer for OAuth2 authorization and content import from another website. _}
        </p>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #description }}" type="text" value="{{ app.description|escape }}" class="form-control" name="description" required placeholder="{_ Description _}">
                <label class="control-label" for="description">{_ Description _}</label>
                {% validate id=#description name="description" type={presence} %}
            </div>
        </div>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #domain }}" type="text" value="{{ app.domain|escape }}" class="form-control" name="domain" required placeholder="{_ Domain (eg. www.example.com) _}">
                <label class="control-label" for="domain">{_ Domain (eg. www.example.com) _}</label>
                {% validate id=#domain name="domain"
                            type={presence}
                            type={format pattern="^[-a-z0-9]+(\\.[-a-z0-9]+)+(:[0-9]+)?$"}
                %}
            </div>
        </div>

        <div class="well">
            <div class="row">
                <div class="col-sm-6">
                    <div class="form-group">
                        <div class="label-floating">
                            <input id="{{ #app_code }}" type="text" value="{{ app.app_code|escape }}" class="form-control" name="app_code" required placeholder="{_ App Code _}">
                            <label class="control-label" for="app_code">{_ App Code_}</label>
                            {% validate id=#app_code name="app_code" type={presence} %}
                        </div>
                    </div>
                </div>
                <div class="col-sm-6">
                    <div class="form-group">
                        <div class="label-floating">
                            <input id="{{ #app_secret }}" type="text" value="{{ app.app_secret|escape }}" class="form-control" name="app_secret" required placeholder="{_ App Secret _}">
                            <label class="control-label" for="app_secret">{_ App Secret _}</label>
                            {% validate id=#app_secret name="app_secret" type={presence} %}
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_use_auth" {% if app.is_use_auth %}checked{% endif %}> {_ Allow users on the remote website to authenticate here _}
            </label>
            <label class="checkbox">
                <input type="checkbox" name="is_use_import" {% if app.is_use_import %}checked{% endif %}> {_ Allow import of content from the remote website _}
            </label>
        </div>

        <p class="help-block">{_ If the remote website is a Zotonic website then you can leave the two URLs below empty. _}</p>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #authorize_url }}" type="text" value="{{ app.authorize_url|escape }}" class="form-control" name="authorize_url" placeholder="{_ Authorize URL _}">
                <label class="control-label" for="authorize_url">{_ Authorize URL _}</label>
            </div>
        </div>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #access_token_url }}" type="text" value="{{ app.access_token_url|escape }}" class="form-control" name="access_token_url" placeholder="{_ Access Token URL _}">
                <label class="control-label" for="access_token_url">{_ Access Token URL _}</label>
            </div>
        </div>

        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
            {% button class="btn btn-primary" type="submit" text=_"Update" %}

            {% if app.token_count > 0 %}
                {% button class="btn btn-danger pull-left" type="submit" text=_"Delete"
                    action={confirm
                        text=[
                            _"Are you sure you want to delete this Consumer App?",
                            "<br>",
                            "<br>",
                            "<b>", _"This will disconnect all tokens and users.", "</b>"
                        ]
                        is_danger
                        ok=_"Delete Consumer App"
                        postback={oauth2_consumer_delete app_id=app.id}
                        delegate=`mod_oauth2`
                    }
                %}
            {% else %}
                {% button class="btn btn-danger pull-left" type="submit" text=_"Delete"
                    action={confirm
                        text=_"Are you sure you want to delete this Consumer App?"
                        is_danger
                        ok=_"Delete Consumer App"
                        postback={oauth2_consumer_delete app_id=app.id}
                        delegate=`mod_oauth2`
                    }
                %}
            {% endif %}
        </div>
    </form>
{% else %}
    <p class="alert alert-error">
        {_ Consumer App not found, or no view permission. _}
    </p>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
    </div>
{% endif %}
