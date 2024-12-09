{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log email _}{% endblock %}

{% block title_log %}{_ Log incoming/outgoing e-mail _}{% endblock %}

{% block active2 %}active{% endblock %}

{% block content_log %}

{#
    -record(log_email, {
    severity = ?LOG_LEVEL_ERROR,
    message_nr,
    mailer_status,      % sending, sent, error, retry, warning, bounce, received
    mailer_message,     % any text, to clarify the mailer_status
    mailer_host,        % SMTP server or client we are talking with
    envelop_to,         % the 'to' on the envelop
    envelop_from,       % the 'from' on the envelop
    to_id,              % who is receiving the e-mail
    from_id,            % who is sending (user in the #context)
    content_id,         % The page being sent (if any)
    other_id,           % In case of a mailinglist the mailinglist id
    message_template,   % template used for rendering the e-mail (if any)
    props = []          % optional extra properties to be logged
    }).
#}

<h3>
    {_ Most recent messages _}
</h3>
<br />

{% if m.modules.active.mod_email_status and q.to and not q.to|match:"%" %}
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header">{_ Email status _}</h3>
                <div class="widget-content" id="{{ #statuspanel }}">
                    {% optional include "_email_status_view.tpl" email=q.to panel_id=#statuspanel %}
                </div>
            </div>
        </div>
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header">{_ Identities _}</h3>
                <div class="widget-content">
                    <h3>{_ Resources with email identity _} &lt;{{ q.to|escape }}&gt;</h3>
                    <br>
                    <ol id="email-identities">
                        {% for idn in m.identity.lookup.email[q.to] %}
                            <li class="{% if not idn.rsc_id.is_a.person %}hidden non-person{% endif %}">
                                <b>
                                    <a href="{% url admin_edit_rsc id=idn.rsc_id %}">
                                        {{ idn.rsc_id.title|default:"untitled" }}
                                    </a>
                                </b>
                                <br>
                                <span class="text-muted">
                                    {{ idn.rsc_id.category_id.title }}
                                    {% if idn.is_verified %}
                                        &mdash; <span class="icon-check"></span> {_ email verified _}
                                    {% endif %}
                                </span>
                            </li>
                        {% endfor %}
                    </ol>
                    <p>
                        <a id="identities-show-all" href="#" class="btn btn-default" style="display:none">{_ Toggle non people _}</a>
                    </p>
                    {% javascript %}
                        if ($('#email-identities li.non-person').length > 0) {
                            $('#identities-show-all').show();
                        }
                        $('#identities-show-all').on('click', function(e) {
                            e.preventDefault();
                            $('#email-identities li.non-person').toggleClass('hidden');
                        });
                    {% endjavascript %}
                </div>
            </div>
        </div>
    </div>
{% endif %}

{% with (q.severity == '0')|if
            :q.severity
            :(q.severity|default:"1")
        as qseverity
%}
{% with m.search[{log_email
    page=q.page
    pagelen=20
    severity=qseverity
    status=q.status
    message_nr=q.message_nr
    to=q.to
    from=q.from
    content=q.content
    other=q.other
    template=q.template
}] as result
%}
<form id="log_filter" method="GET" action="{% url log_email %}">
    <table class="table data-table">
        <thead>
            <tr>
            <th width="8%">{_ Severity _}</th>
            <th width="8%">{_ Status _}</th>
            <th width="8%">{_ Message nr _}</th>
            <th width="15%">{_ To _}</th>
            <th width="15%">{_ From _}</th>
            <th width="5%">{_ Content _}</th>
            <th width="5%">{_ Other _}</th>
            <th width="15%">{_ Template _}</th>
            <th width="20%">{_ Date _}</th>
            </tr>

            <tr>
            <td class="col-lg-1 col-md-1">
            <select class="form-control" id="log_severity" name="severity" style="width: 95%">
                <option value="0" {% if qseverity == '0' %}selected="selected"{% endif %}>{_ Fatal _}</option>
                <option value="1" {% if qseverity == '1' %}selected="selected"{% endif %}>{_ Error _}</option>
                <option value="2" {% if qseverity == '2' %}selected="selected"{% endif %}>{_ Warning _}</option>
                <option value="3" {% if qseverity == '3' %}selected="selected"{% endif %}>{_ Info _}</option>
                <option value="4" {% if qseverity == '4' %}selected="selected"{% endif %}>{_ Debug _}</option>
            </select>
            {% wire id="log_severity" type="change" action={submit target="log_filter"} %}
                </td>
                <td class="col-lg-1 col-md-1">
            <select class="form-control" id="log_status" name="status" style="width: 95%">
                <option value="">{_ All _}</option>
                <option value="sending" {% if q.status == 'sending' %}selected="selected"{% endif %}>{_ Sending _}</option>
                <option value="sent" {% if q.status == 'sent' %}selected="selected"{% endif %}>{_ Sent _}</option>
                <option value="relayed" {% if q.status == 'relayed' %}selected="selected"{% endif %}>{_ Relayed _}</option>
                <option value="bounce" {% if q.status == 'bounce' %}selected="selected"{% endif %}>{_ Bounce _}</option>
                <option value="received" {% if q.status == 'received' %}selected="selected"{% endif %}>{_ Received _}</option>
                <option value="failed" {% if q.status == 'failed' %}selected="selected"{% endif %}>{_ Failed _}</option>
                <option value="retry" {% if q.status == 'retry' %}selected="selected"{% endif %}>{_ Retry _}</option>
                <option value="blocked" {% if q.status == 'blocked' %}selected="selected"{% endif %}>{_ Blocked _}</option>
            </select>
            {% wire id="log_status" type="change" action={submit target="log_filter"} %}
                </td>
            <td class="col-lg-1 col-md-1">
            <input class="form-control" name="message_nr" type="text" style="width: 85%" value="{{ q.message_nr|escape }}" />
                </td>
            <td class="col-lg-2 col-md-2">
            <input class="form-control" name="to" type="text" style="width: 85%" value="{{ q.to|escape }}" />
                </td>
            <td class="col-lg-2 col-md-2">
            <input class="form-control" name="from" type="text" style="width: 85%" value="{{ q.from|escape }}" />
                </td>
            <td class="col-lg-1 col-md-1">
            <input class="form-control" name="content" type="text" style="width: 70%" value="{{ q.content|escape }}" />
                </td>
            <td class="col-lg-1 col-md-1">
            <input class="form-control" name="other" type="text" style="width: 70%" value="{{ q.other|escape }}" />
                </td>
            <td class="col-lg-1 col-md-1">
            <input class="form-control" name="template" type="text" style="width: 85%" value="{{ q.template|escape }}" />
                </td>
            <td class="col-lg-2 col-md-2">
            <button class="btn btn-primary btn-xs" type="submit">{_ Filter _}</button>
            <button class="btn btn-default btn-xs" id="filter_clear">{_ All _}</button>
            {% wire id="filter_clear"
                action={set_value selector="#log_filter input" value=""}
                action={set_value selector="#log_status" value=""}
                action={set_value selector="#log_severity" value="4"}
                action={submit target="log_filter"}
            %}
                </td>
            </tr>
        </thead>

        <tbody id="log-area">
        {% for result_row in result %}
        {% include "_admin_log_email_row.tpl" %}
        {% empty %}
        <tr>
                <td colspan="9">
            {_ No log messages. _}
                </td>
            </tr>
        {% endfor %}
        </tbody>
        <tfoot>
        <tr>
                <td colspan="9">
                {% button
                    class="btn btn-primary"
                    text="More..."
                    action={moreresults
                        result=result
                        target="log-area"
                        template="_admin_log_email_row.tpl"
                    }
                %}
                </td>
            </tr>
        </tfoot>
    </table>
</form>
{% endwith %}
{% endwith %}

{% endblock %}
