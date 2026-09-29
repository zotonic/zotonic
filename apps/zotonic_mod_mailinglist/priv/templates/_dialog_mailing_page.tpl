{% with m.mailinglist_run::%{ page_id:id, list_id:list_id } as history %}
{% with m.mailinglist_run.history_expired[id][list_id] as history_expired %}

{% if options.single_test_address %}
    <p>{_ Test recipient: _} <strong>{{ options.single_test_address|escape }}</strong></p>
{% endif %}

<p>{_ Choose recipients and language, preview the email, then review before sending. _}
{_ Eligible recipients will be counted in the review step. _}</p>

<table class="table table-condensed">
    <tbody>
        <tr>
            <th scope="row">{_ Mailing list _}</th>
            <td>{{ m.rsc[list_id].title|default:_"Untitled" }}</td>
        </tr>
        <tr>
            <th scope="row">{_ Subscribers _}</th>
            <td>
                <span id="{{ #subscriber_count }}" aria-live="polite">{_ Loading… _}</span>
                {% wire action={update target=#subscriber_count template="_mailing_subscriber_count.tpl" list_id=list_id} %}
                <span class="text-muted">{_ before language and other filters _}</span>
                {% if m.rsc[list_id].query %}
                    <p class="help-block">{_ This rough total includes all query matches. Duplicate recipients are not removed yet. _}</p>
                {% endif %}
            </td>
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
    </tbody>
</table>

{% if is_test %}
    <div class="alert alert-info">
        <strong>{_ This test mailing will be sent immediately. _}</strong>
        {_ The page does not need to be published, but you must have permission to view it. _}
    </div>
{% endif %}

{% wire type="submit"
        id=#form
        postback={mailing_page id=id options=options on_success=on_success}
        delegate=delegate
%}
<form id="{{ #form }}" method="post" action="postback">
    <input type="hidden" name="list_id" value="{{ list_id }}" />

    <div class="form-group">
        <label for="{{ #language }}">{_ Email language _}</label>
        <select class="form-control" id="{{ #language }}" name="mailing_language">
            <option value="" {% if not options.language %}selected{% endif %}>
                {_ Choose the best language for each recipient _}
            </option>
            {% for code in id.language %}
                <option value="{{ code|escape }}" {% if options.language == code %}selected{% endif %}>
                    {{ m.translation.language_list_configured[code].name|default:code|escape }}
                </option>
            {% endfor %}
        </select>
    </div>

    <fieldset class="form-group" id="{{ #language_policy_group }}">
        <h4>{_ Who should receive this mailing? _}</h4>
        <div class="radio">
            <label>
                <input type="radio" name="language_policy" value="all" {% if not options.language_policy or options.language_policy == "all" %}checked{% endif %}>
                <strong>{_ Send to everyone _}</strong><br>
                {_ Use their preferred language when available. Otherwise use the fallback language below. _}
            </label>
        </div>
        <div class="radio">
            <label>
                <input type="radio" name="language_policy" value="matching" {% if options.language_policy == "matching" %}checked{% endif %}>
                <strong>{_ Only recipients whose preferred language is available _}</strong><br>
                {_ Skip recipients whose preferred language is unavailable or who have not set a preference. _}
            </label>
        </div>
        <p class="help-block">{_ Regional preferences can use the base language, for example Belgian Dutch can use Dutch. _}</p>
    </fieldset>
    <div class="form-group" id="{{ #fallback_group }}">
        <label for="{{ #fallback }}">{_ Fallback language _}</label>
        <select class="form-control" id="{{ #fallback }}" name="fallback_language">
            {% for code in id.language %}
                <option value="{{ code|escape }}" {% if code == options.fallback_language or not options.fallback_language and code == z_language %}selected{% endif %}>
                    {{ m.translation.language_list_configured[code].name|default:code|escape }}
                </option>
            {% empty %}
                <option value="{{ z_language|escape }}">{{ z_language|escape }}</option>
            {% endfor %}
        </select>
        <p class="help-block">{_ Used when a preferred language is unavailable, unrecognized or not set. _}</p>
    </div>
    <p class="help-block">{_ Unsubscribed recipients, blocked or invalid addresses, and recipients excluded by your delivery-history selection will not receive an email. _}</p>
    <div class="form-group" id="{{ #audience_group }}">
        <label for="{{ #audience }}">{_ Who should receive the mailing in the selected Email language? _}</label>
        <select class="form-control" id="{{ #audience }}" name="audience">
            <option value="matching">{_ Only recipients matching that language _}</option>
            <option value="matching_or_unset" {% if options.audience == "matching_or_unset" %}selected{% endif %}>{_ Recipients matching this language, plus those without a language preference _}</option>
            <option value="all" {% if options.audience == "all" or is_test and not options.audience %}selected{% endif %}>{_ All recipients, using that language _}</option>
        </select>
    </div>
    {% if history and not is_test %}
        <div class="form-group">
            <label for="{{ #mode }}">{_ Previous deliveries _}</label>
            <select class="form-control" id="{{ #mode }}" name="send_mode">
                {% if not is_test and not history_expired %}
                    <option value="new" {% if options.send_mode == "new" %}selected{% endif %}>
                        {_ People who have not received this page in this language _}
                    </option>
                    <option value="failed" {% if options.send_mode == "failed" %}selected{% endif %}>
                        {_ Retry previously failed recipients _}
                    </option>
                {% endif %}
                <option value="all" {% if options.send_mode == "all" %}selected{% endif %}>
                    {_ Send again to everyone _}
                </option>
            </select>
            <p class="help-block">{_ Delivery history is preserved. Sending again may result in recipients receiving the same content twice. Older mailings from before run tracking are not used for duplicate detection. _}</p>
        </div>
    {% else %}
        <input type="hidden" name="send_mode" value="{% if is_test %}all{% else %}new{% endif %}">
    {% endif %}
    {% if history_expired %}
        <p class="alert alert-warning">{_ Some recipient history has expired after three months. We can no longer identify everyone who received this page. Only sending to all selected recipients is available; some people may receive it again. _}</p>
    {% endif %}

    <h4>{_ Preview and test _}</h4>
    <p>{_ Open a preview in a new tab before continuing: _}
        {% for code in id.language %}
            <a target="_mailingpreview" title="{_ Opens in a new tab _}" href="{% url admin_mailing_preview id=id z_language=code %}">
                {{ m.translation.language_list_configured[code].name|default:code|escape }}
                <span aria-hidden="true">↗</span>
            </a>
        {% endfor %}
    </p>
    {% if not is_test %}
        {% if test_run_id %}
            <p class="alert alert-info">{_ Test email queued for _} {{ test_email|escape }}.
            <a target="_mailingtest" href="{% url admin_mailing_run run_id=test_run_id %}">{_ View test result _}</a></p>
        {% endif %}

        <div class="alert alert-info">
            <details>
                <summary>{_ Send a test email before continuing _}</summary>
                <div class="form-group">
                    <label for="{{ #test_email }}">{_ Test email address _}</label>
                    <input class="form-control" id="{{ #test_email }}" name="test_email" type="email" value="{{ test_email|default:m.acl.user.email|escape }}">
                </div>
                <div class="form-group">
                    <label for="{{ #test_language }}">{_ Test language _}</label>
                    <select class="form-control" id="{{ #test_language }}" name="test_language">
                        {% for code in id.language %}
                            <option value="{{ code|escape }}" {% if test_language == code %}selected{% endif %}>
                                {{ m.translation.language_list_configured[code].name|default:code|escape }}
                            </option>
                        {% endfor %}
                    </select>
                </div>

                <button class="btn btn-default" type="submit" name="mailing_step" value="test">{_ Send test email _}</button>

                <p class="help-block">{_ Only the test address receives this email. Your mailing choices are kept here. _}</p>
            </details>
        </div>

        {% with not m.rsc[id].is_published or m.rsc[id].publication_start|in_future as is_unpublished %}
            <div class="form-group">
                <p><strong>{_ When should the mailing be sent? _}</strong></p>

                <label class="radio">
                    <input type="radio" name="mail_when" value="now" {% if not mail_when or mail_when == "now" or mail_when == "scheduled" %}checked{% endif %}>
                    {% if is_unpublished %}
                        {_ Send as soon as the page is published. _}
                        {% if m.rsc[id].publication_start|in_future %}
                            <span class="text-muted">{_ Not before _} {{ m.rsc[id].publication_start|date:_"Y-m-d H:i" }} ({{ m.req.timezone|escape }}).</span>
                        {% endif %}
                    {% else %}
                        {_ Send the mailing right now. _}
                    {% endif %}
                </label>
                <label class="radio">
                    <input type="radio" name="mail_when" value="date" {% if mail_when == "date" %}checked{% endif %}>
                    {_ Send the mailing on a specific date and time. _}
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
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
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
    const languagePolicyGroup = document.getElementById("{{ #language_policy_group }}");
    const toggleLanguage = () => {
        document.getElementById("{{ #audience_group }}").hidden = !languageSelect.value;
        languagePolicyGroup.hidden = !!languageSelect.value;
        const matchingOnly = languagePolicyGroup.querySelector("input[value='matching']").checked;
        document.getElementById("{{ #fallback_group }}").hidden = !!languageSelect.value || matchingOnly;
    };
    languageSelect.addEventListener("change", toggleLanguage);
    languagePolicyGroup.addEventListener("change", toggleLanguage);
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
