        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #description }}" type="text" value="{{ app.description|escape }}" class="form-control" name="description" required placeholder="{_ Description - shows up on button _}">
                <label class="control-label" for="description">{_ Description - shows up on button _}</label>
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
                            <label class="control-label" for="app_code">{_ App ID _}</label>
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
            <label class="control-label" for="access_token_url">{_ Token grant method _}</label>
            <select class="form-control" name="grant_type" style="max-width: 30ch">
                <option value="authorization_code">
                    Authorization Code ({_ default _})
                </option>
                <option value="client_credentials" {% if app.grant_type == 'client_credentials' %}selected{% endif %}>
                    Client Credentials
                </option>
            </select>
        </div>
        <p class="help-block">
            {% trans "The “{code}” method redirects the user to the remote website to obtain an access token. “{client}” allows an admin user to directly fetch a token from the remote website."
                code="Authorization Code"
                client="Client Credentials"
            %}
        </p>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_use_auth" {% if app.is_use_auth %}checked{% endif %}>
                {% trans "Allow users on the remote website to authenticate here (using “{code}”)"
                        code="Authorization Code"
                %}
            </label>
            <label class="checkbox">
                <input type="checkbox" name="is_use_import" {% if app.is_use_import %}checked{% endif %}> {_ Allow import of content from the remote website _}
            </label>
            <label class="checkbox">
                <input type="checkbox" value="1" name="is_extend_automatic" {% if app.is_extend_automatic %}checked{% endif %}>
                {% trans "Automatically extend tokens obtained using “{client}” before they expire"
                        client="Client Credentials"
                %}
            </label>
        </div>

        <p class="help-block">{_ If the remote website is a Zotonic website then you can leave the two URLs below empty. _}</p>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #authorize_url }}" type="text" value="{{ app.authorize_url|escape }}" class="form-control" name="authorize_url" placeholder="{_ Authorize URL _}">
                <label class="control-label" for="authorize_url">{_ Authorize URL _}</label>
                <p class="help-block">{% trans "Leave empty if you use “{client}.”" client="Client Credentials" %}</p>
            </div>
        </div>

        <div class="form-group">
            <div class="label-floating">
                <input id="{{ #access_token_url }}" type="text" value="{{ app.access_token_url|escape }}" class="form-control" name="access_token_url" placeholder="{_ Access Token URL _}">
                <label class="control-label" for="access_token_url">{_ Access Token URL _}</label>
            </div>
        </div>
