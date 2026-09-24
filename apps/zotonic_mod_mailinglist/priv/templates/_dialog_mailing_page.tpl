{% with m.mailinglist.stats[list_id] as list_stats %}
{% with m.mailinglist_run::%{page_id:id, list_id:list_id} as history %}

{% if options.single_test_address %}<p>{_ Test recipient: _} <strong>{{ options.single_test_address|escape }}</strong></p>{% endif %}
<p>{_ Choose recipients and language, preview the email, then review before sending. _}</p>

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
            <th scope="row">{_ List members before selection _}</th>
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

{% wire type="submit" id=#form postback={mailing_page id=id options=options on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">
    <input type="hidden" name="list_id" value="{{ list_id }}" />

    <div class="form-group">
        <label for="{{ #language }}">{_ Language _}</label>
        <select class="form-control" id="{{ #language }}" name="mailing_language">
            <option value="" {% if not options.language %}selected{% endif %}>{_ Recipient's preferred language _}</option>
            {% for code in id.language %}<option value="{{ code|escape }}" {% if options.language == code %}selected{% endif %}>{{ m.translation.language_list_configured[code].name|default:code|escape }}</option>{% endfor %}
        </select>
    </div>
    <div class="form-group" id="{{ #fallback_group }}">
        <label for="{{ #fallback }}">{_ Language for recipients without a preference _}</label>
        <select class="form-control" id="{{ #fallback }}" name="fallback_language">
            {% for code in id.language %}<option value="{{ code|escape }}" {% if code == options.fallback_language or not options.fallback_language and code == z_language %}selected{% endif %}>{{ m.translation.language_list_configured[code].name|default:code|escape }}</option>
            {% empty %}<option value="{{ z_language|escape }}">{{ z_language|escape }}</option>{% endfor %}
        </select>
        <p class="help-block">{_ Missing translations are skipped and reported. _}</p>
    </div>
    <div class="form-group" id="{{ #audience_group }}">
        <label for="{{ #audience }}">{_ When selecting a language _}</label>
        <select class="form-control" id="{{ #audience }}" name="audience">
            <option value="matching">{_ Only recipients matching that language _}</option>
            <option value="all" {% if options.audience == "all" or is_test and not options.audience %}selected{% endif %}>{_ All recipients, using that language _}</option>
        </select>
    </div>
    {% if history and not is_test %}
    <div class="form-group">
        <label for="{{ #mode }}">{_ Recipients _}</label>
        <select class="form-control" id="{{ #mode }}" name="send_mode">
            {% if not is_test %}
            <option value="new" {% if options.send_mode == "new" %}selected{% endif %}>{_ People who have not received this page in this language _}</option>
            <option value="failed" {% if options.send_mode == "failed" %}selected{% endif %}>{_ Retry previously failed recipients _}</option>
            {% endif %}
            <option value="all" {% if options.send_mode == "all" %}selected{% endif %}>{_ Send again to everyone _}</option>
        </select>
        <p class="help-block">{_ Delivery history is preserved. Sending again may result in recipients receiving the same content twice. Older mailings from before run tracking are not used for duplicate detection. _}</p>
    </div>
    {% else %}<input type="hidden" name="send_mode" value="{% if is_test %}all{% else %}new{% endif %}">{% endif %}
    <h4>{_ Preview and test _}</h4>
    <p>{_ Open a preview before continuing: _}
        {% for code in id.language %}<a target="_mailingpreview" href="{% url admin_mailing_preview id=id z_language=code %}">{{ m.translation.language_list_configured[code].name|default:code|escape }}</a> {% endfor %}
    </p>
    {% if not is_test %}
    {% if test_run_id %}<p class="alert alert-info">{_ Test email queued for _} {{ test_email|escape }}.
        <a target="_mailingtest" href="{% url admin_mailing_run run_id=test_run_id %}">{_ View test result _}</a></p>{% endif %}
    <details><summary>{_ Send a test email before continuing _}</summary>
        <label for="{{ #test_email }}">{_ Test email address _}</label>
        <input class="form-control" id="{{ #test_email }}" name="test_email" type="email" value="{{ test_email|default:m.acl.user.email|escape }}">
        <label for="{{ #test_language }}">{_ Test language _}</label>
        <select class="form-control" id="{{ #test_language }}" name="test_language">
            {% for code in id.language %}<option value="{{ code|escape }}" {% if test_language == code %}selected{% endif %}>{{ m.translation.language_list_configured[code].name|default:code|escape }}</option>{% endfor %}
        </select>
        <button class="btn btn-default" type="submit" name="mailing_step" value="test">{_ Send test email _}</button>
        <p class="help-block">{_ Only the test address receives this email. Your mailing choices are kept here. _}</p>
    </details>
    {% endif %}
    {% if not is_test %}
        {% with not m.rsc[id].is_published or m.rsc[id].publication_start|in_future as is_unpublished %}
            <div class="form-group">
                <p><strong>{_ When should the mailing be sent? _}</strong></p>

                <label class="radio">
                    <input type="radio" name="mail_when" value="now" {% if mail_when == "now" or not mail_when and not is_unpublished %}checked="checked" {% endif %}> {_ Send the mailing right now. _}
                    {% if is_unpublished %}
                        <span class="text-muted">{_ The page is unpublished, no link back to the website in the mailing. _}</span>
                    {% endif %}
                </label>
                {% if is_unpublished %}
                    <label class="radio">
                        <input type="radio" name="mail_when" value="scheduled" {% if not mail_when or mail_when == "scheduled" %}checked{% endif %}>
                        {% if not m.rsc[id].is_published %}
                            {_ Send the mailing immediately after the page has been published. _}
                        {% else %}
                            {_ Send the mailing automatically after the publication start date of _} {{ m.rsc[id].publication_start|date:_"Y-m-d H:i" }}.
                        {% endif %}
                    </label>
                {% endif %}
                <label class="radio">
                    <input type="radio" name="mail_when" value="date" {% if mail_when == "date" %}checked{% endif %}> {_ Send the mailing on a specific date and time. _}
                </label>
                <div id="{{ #send_date }}" style="margin-left: 15px" hidden>
                    <label>{_ Send date: _}</label>
                    <input type="date"
                           id="{{ #mailing_date }}"
                           value="{{ mailing_date|escape }}" name="dt:ymd:0:mailing_date"
                           class="form-control">
                    <input type="time"
                           id="{{ #mailing_time }}"
                           value="{{ mailing_time|escape }}" name="dt:hi:0:mailing_date"
                           class="form-control">
                    <span class="text-muted">{{ m.req.timezone }}</span>
                </div>
            </div>
        {% endwith %}
    {% endif %}

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% if is_test %}
            {% button class="btn btn-primary" type="submit" text=_"Review test mailing" %}
        {% else %}
            {% button class="btn btn-primary" type="submit" text=_"Review mailing" %}
        {% endif %}

        <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default pull-left">{_ Edit page _}</a>
        <a href="{% url admin_edit_rsc id=list_id %}" class="btn btn-default pull-left">{_ Edit mailinglist _}</a>
    </div>
</form>

{% javascript %}
    const languageSelect = document.getElementById("{{ #language }}");
    const toggleLanguage = () => {
        document.getElementById("{{ #audience_group }}").hidden = !languageSelect.value;
        document.getElementById("{{ #fallback_group }}").hidden = !!languageSelect.value;
    };
    languageSelect.addEventListener("change", toggleLanguage);
    toggleLanguage();
{% endjavascript %}
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
