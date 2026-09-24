<h4>{_ Review before sending _}</h4>
<p><strong>{{ m.rsc[id].title }}</strong> → {{ m.rsc[list_id].title }}</p>
{% if is_test %}<p class="label label-info">{_ Test email _}</p>{% endif %}
{% if options.single_test_address %}<p>{_ Test recipient: _} <strong>{{ options.single_test_address|escape }}</strong></p>{% endif %}
<p><strong>{{ eligible }} {_ recipients selected. _}</strong>
    {% if options.language %}{_ Send the _} {{ m.translation.language_list_configured[options.language].name|default:options.language|escape }} {_ version. _}
    {% else %}{_ Use each recipient’s preferred language. _}{% endif %}</p>
<p>{% if type == "publication" %}{_ Send when the page becomes published. _}
    {% elseif mail_when == "now" %}{_ Send immediately after confirmation. _}
    {% else %}{_ Scheduled for: _} {{ due|date:"Y-m-d H:i" }} ({{ m.req.timezone|escape }}){% endif %}</p>
<table class="table"><thead><tr><th>{_ Language _}</th><th>{_ Selection _}</th><th>{_ Recipients _}</th></tr></thead><tbody>
{% for row in counts %}<tr><td>{{ m.translation.language_list_configured[row.language].name|default:row.language|escape }}</td><td>{% if row.status == "pending" %}{_ Eligible to send _}{% else %}{_ Skipped _}{% endif %}</td><td>{{ row.total }}</td></tr>
{% empty %}<tr><td colspan="3">{_ No recipients found. _}</td></tr>{% endfor %}
</tbody></table>
{% if reasons %}<details open><summary>{_ Why some recipients are not selected _}</summary>
    <ul>{% for reason,total in reasons %}<li>{{ total }} — {% include "_mailing_skip_reason.tpl" reason=reason %}</li>{% endfor %}</ul>
</details>{% endif %}
<p>{_ These counts are estimates. Recipients and translations are checked again when sending starts. _}</p>
{% if options.send_mode == "all" and not is_test %}<p class="alert alert-warning">{_ This sends again to everyone selected, including recipients who received this page before. _}</p>{% endif %}
{% if not eligible %}<p class="alert alert-warning">{_ Nothing to send. Go back and change the language or recipient selection, or choose another mailing list. _}</p>{% endif %}
<div class="modal-footer">
{% button class="btn btn-default" text=_"Back" postback={mailing_back id=id list_id=list_id options=options mail_when=mail_when mailing_date=mailing_date mailing_time=mailing_time} delegate="action_mailinglist_dialog_mailing_page" %}
{% if eligible %}
    {% if mail_when == "now" %}
        {% button class="btn btn-primary" text=_"Send mailing now"
            postback={mailing_confirm page_id=id list_id=list_id type=type due=due options=options}
            delegate="action_mailinglist_dialog_mailing_page" %}
    {% else %}
        {% button class="btn btn-primary" text=_"Schedule mailing"
            postback={mailing_confirm page_id=id list_id=list_id type=type due=due options=options}
            delegate="action_mailinglist_dialog_mailing_page" %}
    {% endif %}
{% endif %}
</div>
