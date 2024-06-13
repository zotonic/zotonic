{% with m.mailinglist.rsc_stats[id] as rsc_stats %}

<p>
    {% trans "Please confirm that you want to send the page <i>{page}</i> to the list <i>{list}</i>."
            page=m.rsc[id].title
            list=m.rsc[list_id].title
    %}
</p>

{% wire type="submit" id=#form postback={mailing_page id=id on_success=on_success} action={dialog_close} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">
    <input type="hidden" name="list_id" value="{{ list_id }}" />

    {% if rsc_stats[list_id].total > 0 %}
    <div class="alert alert-info">
    <p><strong>{_ Please note: _}</strong> {_ It appears you have sent
        this page once already to this list. If you send it again, only
        the recipients that did not yet receive the mail will get it. As a
        safety-caution, it is impossible to send the same page twice to
        the same e-mail address. _}</p>

        <div class="form-group">
            <label class="checkbox">
                <input type="checkbox" name="is_send_all" value="1">
                {_ Forget previous e-mails, mail all recipients again. _}
            </label>
        </div>
    </div>
    {% endif %}

    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" name="is_match_language" value="1">
            {_ Only send to recipients whose preferred language match the translations of the mailing. _}
        </label>
    </div>

    {% if not m.rsc[id].is_published or m.rsc[id].publication_start|in_future %}
        <div class="form-group">
            <p>{_ The page you are trying to e-mail is not yet published. What do you want to do? _}</p>

            <label class="radio">
                <input type="radio" name="mail_when" value="now" checked="checked"> {_ Send the mailing right now, but do not include a link back to the website. _}
            </label>
            <label class="radio">
                <input type="radio" name="mail_when" value="scheduled">
                {% if not m.rsc[id].is_published %}
                    {_ Send the mailing immediately after the "published" checkbox has been checked in the edit page. _}
                {% else %}
                    {_ Send the mailing automatically after the publication start date of _} {{ m.rsc[id].publication_start|date:_"d M Y, H:i" }}.
                {% endif %}
            </label>
        </div>
    {% endif %}

    <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
            {% button class="btn btn-primary" type="submit" text=_"Send mailing" %}
    </div>
</form>

{% endwith %}
