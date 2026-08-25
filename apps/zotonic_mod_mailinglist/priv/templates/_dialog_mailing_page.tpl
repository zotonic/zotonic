{% with m.mailinglist.rsc_stats[id] as rsc_stats %}
{% with m.mailinglist.stats[list_id] as list_stats %}

<p>{_ Are you sure you want to send this mailing? _}</p>

<table class="table table-condensed">
    <tbody>
        <tr>
            <th scope="row">{_ Mailing list _}</th>
            <td>{{ m.rsc[list_id].title|default:_"Untitled" }}</td>
        </tr>
        <tr>
            <th scope="row">{_ Page _}</th>
            <td>
                {{ m.rsc[id].title|default:_"Untitled" }}
                {% if not m.rsc[id].is_published %}
                    <span class="label label-warning">{_ Unpublished _}</span>
                {% endif %}
            </td>
        </tr>
        {% if m.rsc[id].publication_start|in_future %}
            <tr>
                <th scope="row">{_ Publication date _}</th>
                <td>{{ m.rsc[id].publication_start|date:_"Y-m-d H:i" }}</td>
            </tr>
        {% endif %}
        <tr>
            <th scope="row">{_ Recipients _}</th>
            <td>{{ list_stats.total|default:0 }}</td>
        </tr>
    </tbody>
</table>

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
            <p>
                <strong>{_ Please note: _}</strong>
                {_ This page has already been sent to this list. If you send it again, only recipients who have not yet received it will be emailed. _}
            </p>

            <div class="form-group">
                <label class="checkbox">
                    <input type="checkbox" name="is_send_all" value="1"> {_ Clear the delivery history and send to all recipients again. _}
                </label>
            </div>
        </div>
    {% endif %}

    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" name="is_match_language" value="1">
            {_ For recipients with a preferred language, only send if this page has a matching translation. _}
        </label>
    </div>

    {% if not is_test %}
        {% with not m.rsc[id].is_published or m.rsc[id].publication_start|in_future as is_unpublished %}
            <div class="form-group">
                <p><strong>{_ When should the mailing be sent? _}</strong></p>

                <label class="radio">
                    <input type="radio" name="mail_when" value="now" {% if not is_unpublished %}checked="checked" {% endif %}> {_ Send the mailing right now. _}
                    {% if is_unpublished %}
                        <span class="text-muted">{_ The page is unpublished, no link back to the website in the mailing. _}</span>
                    {% endif %}
                </label>
                {% if is_unpublished %}
                    <label class="radio">
                        <input type="radio" name="mail_when" value="scheduled" checked="checked">
                        {% if not m.rsc[id].is_published %}
                            {_ Send the mailing immediately after the page has been published. _}
                        {% else %}
                            {_ Send the mailing automatically after the publication start date of _} {{ m.rsc[id].publication_start|date:_"Y-m-d H:i" }}.
                        {% endif %}
                    </label>
                {% endif %}
                <label class="radio">
                    <input type="radio" name="mail_when" value="date"> {_ Send the mailing on a specific date and time. _}
                </label>
                <div id="{{ #send_date }}" style="margin-left: 15px" hidden>
                    <label>{_ Send date: _}</label>
                    <input type="date"
                           id="{{ #mailing_date }}"
                           name="dt:ymd:0:mailing_date"
                           class="form-control">
                    <input type="time"
                           id="{{ #mailing_time }}"
                           name="dt:hi:0:mailing_date"
                           class="form-control">
                    <span class="text-muted">{{ m.req.timezone }}</span>
                </div>
            </div>
        {% endwith %}
    {% endif %}

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% if is_test %}
            {% button class="btn btn-primary" type="submit" text=_"Send test mailing now" %}
        {% else %}
            {% button class="btn btn-primary" type="submit" text=_"Send mailing" %}
        {% endif %}

        <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default pull-left">{_ Edit page _}</a>
        <a href="{% url admin_edit_rsc id=list_id %}" class="btn btn-default pull-left">{_ Edit mailinglist _}</a>
    </div>
</form>

{% if not is_test %}
{% javascript %}
    const form = document.getElementById("{{ #form }}");
    const mailWhenDate = form.querySelector("input[name='mail_when'][value='date']");
    const sendDate = document.getElementById("{{ #send_date }}");
    const toggleSendDate = () => {
        sendDate.hidden = !mailWhenDate.checked;
    };

    form.querySelectorAll("input[name='mail_when']")
        .forEach((input) => input.addEventListener("change", toggleSendDate));

    [document.getElementById("{{ #mailing_date }}"), document.getElementById("{{ #mailing_time }}")]
        .forEach((input) => input.addEventListener("input", () => {
            mailWhenDate.checked = true;
            toggleSendDate();
        }));

    toggleSendDate();
{% endjavascript %}
{% endif %}

{% endwith %}
{% endwith %}
