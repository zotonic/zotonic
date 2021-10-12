{% wire id=#new
        type="submit"
        postback={oauth2_consumer_insert}
        delegate=`mod_oauth2`
%}
<form id="{{ #new }}" action="postback">
    <p>
        {_ Make a new consumer for OAuth2 authorization and content import from another website. _}
    </p>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #name }}" type="text" value="{{ app.name|escape }}" class="form-control" name="name" required autofocus placeholder="{_ Name _}" maxlength="128">
            <label class="control-label" for="name">{_ Name _}</label>
            {% validate id=#name name="name"
                        type={presence}
                        type={format pattern="^[-_a-zA-Z0-9]+$"}
            %}
            <p class="help-block">{_ This must be an unique name to identify the remote service. This can not be changed. Only a-z and 0-9 are allowed. _}</p>
        </div>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #description }}" type="text" value="" class="form-control" name="description" required placeholder="{_ Description _}">
            <label class="control-label" for="description">{_ Description _}</label>
            {% validate id=#description name="description" type={presence} %}
        </div>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #domain }}" type="text" value="" class="form-control" name="domain" required placeholder="{_ Domain (eg. www.example.com) _}">
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
                        <input id="{{ #app_code }}" type="text" value="" class="form-control" name="app_code" required placeholder="{_ App Code _}">
                        <label class="control-label" for="app_code">{_ App Code_}</label>
                        {% validate id=#app_code name="app_code" type={presence} %}
                    </div>
                </div>
            </div>
            <div class="col-sm-6">
                <div class="form-group">
                    <div class="label-floating">
                        <input id="{{ #app_secret }}" type="text" value="" class="form-control" name="app_secret" required placeholder="{_ App Secret _}">
                        <label class="control-label" for="app_secret">{_ App Secret _}</label>
                        {% validate id=#app_secret name="app_secret" type={presence} %}
                    </div>
                </div>
            </div>
        </div>
    </div>

    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" name="is_use_auth"> {_ Allow users on the remote website to authenticate here _}
        </label>
        <label class="checkbox">
            <input type="checkbox" name="is_use_import"> {_ Allow import of content from the remote website _}
        </label>
    </div>

    <p class="help-block">{_ If the remote website is a Zotonic website then you can leave the two URLs below empty. _}</p>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #authorize_url }}" type="text" value="" class="form-control" name="authorize_url" placeholder="{_ Authorize URL _}">
            <label class="control-label" for="authorize_url">{_ Authorize URL _}</label>
        </div>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #access_token_url }}" type="text" value="" class="form-control" name="access_token_url" placeholder="{_ Access Token URL _}">
            <label class="control-label" for="access_token_url">{_ Access Token URL _}</label>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Register" %}
    </div>
</form>
