{% with m.mailinglist.rsc_stats[id] as rsc_stats %}

<p>
    {% if is_test %}
        {% trans "Please confirm that you want to send the page <i>{page}</i> to the test mailing list <i>{list}</i>."
                page=m.rsc[id].title
                list=m.rsc[list_id].title
        %}
    {% else %}
        {% trans "Please confirm that you want to send the page <i>{page}</i> to the list <i>{list}</i>."
                page=m.rsc[id].title
                list=m.rsc[list_id].title
        %}
    {% endif %}
</p>

{% if is_test %}
    <div class="alert alert-info">
        <strong>{_ This test mailing will be sent immediately. _}</strong>
        {_ The page does not need to be published, but you must have permission to view it. _}
    </div>
{% endif %}

{% wire type="submit" id=#form postback={mailing_page id=id on_success=on_success} delegate=delegate %}
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

    {% if not is_test %}
    <div class="form-group">
        <p><strong>{_ When should the mailing be sent? _}</strong></p>

        <label class="radio">
            <input type="radio" name="mail_when" value="now" checked="checked">
            {% if not m.rsc[id].is_published or m.rsc[id].publication_start|in_future %}
                {_ Send the mailing right now, but do not include a link back to the website. _}
            {% else %}
                {_ Send the mailing right now. _}
            {% endif %}
        </label>

        <label class="radio">
            <input type="radio" name="mail_when" value="date"> {_ Send the mailing on a specific date and time. _}
        </label>
        <div class="row">
            <div class="col-sm-6">
                <label for="{{ #mailing_date }}">{_ Mailing date _}</label>
                <input type="date"
                       id="{{ #mailing_date }}"
                       name="dt:ymd:0:mailing_date"
                       class="form-control">
            </div>
            <div class="col-sm-6">
                <label for="{{ #mailing_time }}">{_ Mailing time _}</label>
                <input type="time"
                       id="{{ #mailing_time }}"
                       name="dt:hi:0:mailing_date"
                       class="form-control">
            </div>
        </div>
        <p class="help-block">
            {% trans "The date and time use the {timezone} time zone." timezone=m.req.timezone %}
        </p>

        {% if not m.rsc[id].is_published or m.rsc[id].publication_start|in_future %}
            <label class="radio">
                <input type="radio" name="mail_when" value="scheduled">
                {% if not m.rsc[id].is_published %}
                    {_ Send the mailing immediately after the "published" checkbox has been checked in the edit page. _}
                {% else %}
                    {_ Send the mailing automatically after the publication start date of _} {{ m.rsc[id].publication_start|date:_"d M Y, H:i" }}.
                {% endif %}
            </label>
        {% endif %}
    </div>
    {% endif %}

    <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
            {% if is_test %}
                {% button class="btn btn-primary" type="submit" text=_"Send test mailing now" %}
            {% else %}
                {% button class="btn btn-primary" type="submit" text=_"Send mailing" %}
            {% endif %}
    </div>
</form>

{% if not is_test %}
{% javascript %}
    [document.getElementById("{{ #mailing_date }}"), document.getElementById("{{ #mailing_time }}")]
        .forEach((input) => input.addEventListener("input", () => {
            document.querySelector("#{{ #form }} input[name='mail_when'][value='date']").checked = true;
        }));
{% endjavascript %}
{% endif %}

{% endwith %}
