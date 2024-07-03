<p>{_ Add all email addresses to a mailinglist. Users are subscribed by connecting them to the mailinglist. _}</p>

<fieldset>
    <legend>{_ Add to existing mailinglist _}</legend>
    {% wire id=#mailinglist_select
            type="submit"
            postback={mailinglist_add id=id}
            delegate=`survey_admin`
    %}
    <form id="{{ #mailinglist_select }}" class="form-inline" action="postback">
        <select class="form-control" name="mailinglist_id" required>
            <option value="">{_ Select mailinglist _}</option>
            {% for title, id in m.search[{all_bytitle cat=`mailinglist` pagelen=1000}] %}
                {% if id.is_editable %}
                    <option value="{{ id }}">{{ id.title }}</option>
                {% else %}
                    <option value="{{ id }}" disabled>{{ id.title }}</option>
                {% endif %}
            {% endfor %}
        </select>
        <button type="submit" class="btn btn-primary">{_ Add email addresses _}</button>
    </form>
</fieldset>

<p><br></p>

<fieldset>
    <legend>{_ Create new mailinglist _}</legend>
    {% wire id=#mailinglist_new
            type="submit"
            postback={mailinglist_new id=id}
            delegate=`survey_admin`
    %}
    <form id="{{ #mailinglist_new }}" class="form" action="postback">
        <div class="form-group label-floating">
            <input class="form-control" type="text" name="title" value="{_ Mailinglist _}: {{ id.title }}" placeholder="{_ Mailinglist title _}" required>
            <label class="control-label">{_ Mailinglist title _}</label>
        </div>

        {% include "_admin_edit_visible_for.tpl" %}

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_published" checked> {_ Published _}
            </label>
        </div>

        <div class="modal-footer">
            {% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
            <button type="submit" class="btn btn-primary">{_ Create mailinglist _}</button>
        </div>
    </form>
</fieldset>

