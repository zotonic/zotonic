{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing status _} &mdash; {{ m.rsc[id].title }}{% endblock %}

{% block content %}
<div class="edit-header">

    <p class="admin-chapeau">{_ mailinglist status page _}:</p>
    <h2>{{ m.rsc[id].title }}</h2>

    <p>{_ This page can be sent to different mailing lists. _}</p>
    
    <div class="well">
        <a class="btn btn-primary" href="{% url admin_edit_rsc id=id %}" class="button">{_ edit _}</a>
        <a class="btn"  href="{% url admin_mailing_preview id=id %}" class="button" onclick="window.open(this.getAttribute('href'), 'mailingpreview', 'width=800,height=800');return false;">{_ preview mailing _}</a>
        {% button text=_"Send test mailing" class="btn" title=_"Send this page to the test mailing list." action={mailing_page_test id=id} %}
        {% button text=_"Send test to address" class="btn" title=_"Send this page to a single address" action={dialog_open template="_dialog_mailing_testaddress.tpl" title=_"Send test to address" id=id} %}

        <a href="javascript:void(0)" class="btn do_dialog" data-dialog="title: '{{ _"Help about the mailing page."|escapejs }}', text: '{{ _"This overview allows you to send the current page to a group of recipients, grouped into mailing lists. Choose 'preview mailing' to open a popup window which shows how the mailing will look like when it is sent; choose 'send test mailing' to send it to the predefined list of test e-mail addresses. Choose 'edit' to go back to editing the page.  Each mailinglist is listed in the table below, together with statistics on when it was sent and to how many recipients."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign"></i></a>
            </div>

    </div>

    <div class="zp-100" id="mailing-status">
        {% include "_admin_mailing_status_overview.tpl" %}
    </div>

    {% wire action={connect signal={log_email} action={update target="mailing-status" template="_admin_mailing_status_overview.tpl" id=id}} %}
    {% wire action={connect signal={update_mailinglist_scheduled} action={update target="mailing-status" template="_admin_mailing_status_overview.tpl" id=id}} %}
</div>
{% endblock %}
