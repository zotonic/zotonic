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
            <input id="{{ #name }}" type="text" value="{{ app.name|escape }}" class="form-control" name="name" required autofocus placeholder="{_ Name _}" maxlength="40">
            <label class="control-label" for="name">{_ Name _}</label>
            {% validate id=#name name="name"
                        type={presence}
                        type={format pattern="^[-_a-zA-Z0-9]+$"}
            %}
            <p class="help-block">{_ This must be an unique name to identify the remote service. This can not be changed. Only <tt>A-Z</tt>, <tt>a-z</tt>, <tt>0-9</tt>, <tt>-</tt> and <tt>_</tt> characters are allowed. _}</p>
        </div>
    </div>

    {% include "_oauth2_consumer_fields.tpl" app=%{} %}

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Register" %}
    </div>
</form>
