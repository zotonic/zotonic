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
                    <p>
                        <button id="{{ #generate }}" class="btn btn-default">{_ Generate my access token _}</button>
                        {% wire id=#generate
                                action={confirm
                                    text=_"If you have already a token then it will be replaces with the new one."
                                    ok=_"Generate access token"
                                    postback={oauth2_app_token_generate app_id=app.id}
                                    delegate=`mod_oauth2`
                                }
                        %}
                    </p>
                </div>
                <div class="col-sm-6">
                    <p>
                        <label>{_ App secret _}</label><br>
                        <tt>{{ app.app_secret|escape }}</tt>
                    </p>
                </div>
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
    <p class="alert alert-error">
        {_ App not found, or no view permission. _}
    </p>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
    </div>
{% endif %}
