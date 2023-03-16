{% wire id=#new
        type="submit"
        postback={oauth2_app_insert}
        delegate=`mod_oauth2`
%}
<form id="{{ #new }}" action="postback">
    <p>
        {_ Make a new App for OAuth2 authorization and/or content export to other websites. _}
    </p>

    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" name="is_enabled"> {_ Enabled _}
        </label>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #description }}" type="text" value="" class="form-control" autofocus name="description" required placeholder="{_ Description _}">
            <label class="control-label" for="description">{_ Description _}</label>
            {% validate id=#description name="description" type={presence} %}
        </div>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <textarea id="{{ #redirect_urls }}" class="form-control" name="redirect_urls" required placeholder="{_ Valid redirect URLs, one per line _}"></textarea>
            <label class="control-label" for="redirect_urls">{_ Valid redirect URLs, one per line _}</label>
            {% validate id=#redirect_urls name="redirect_urls" type={presence} %}
            <p class="help-block">
                {_ Give the redirect URLs that are valid for the website performing the OAuth2 authorization. _}
                {_ These must be complete URLs, but without the query (?..) or hash (#...) parts. _}<br>
                {_ For Zotonic sites you can enter the domain name(s) of the website that wants to access the data. _}
            </p>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Make App" %}
    </div>
</form>
