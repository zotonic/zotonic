{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing status _} &mdash; {{ m.rsc[id].title }}{% endblock %}

{% block content %}
<div class="admin-header">

    <p class="admin-chapeau">{_ mailinglist status page _}:</p>
    <h2>{{ m.rsc[id].title }}</h2>

    <p>{_ This page can be sent to different mailing lists. _} <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Help about the mailing page",
                text: _"This overview allows you to send the current page to a group of recipients, grouped into mailing lists. Choose 'preview mailing' to open a popup window which shows how the mailing will look like when it is sent; choose 'send test mailing' to send it to the predefined list of test e-mail addresses. Choose 'edit' to go back to editing the page.  Each mailinglist is listed in the table below, together with statistics on when it was sent and to how many recipients."
            }|escape
        }}" title="{_ Need more help? _}"></a>
    </p>
    <div class="well">
        <a class="btn btn-primary" href="{% url admin_edit_rsc id=id %}" class="button">{_ Edit _}</a>
        <a class="btn btn-default"  href="{% url admin_mailing_preview id=id %}" class="button" id="mailing-preview-btn">{_ Preview mailing _}</a>
        {% button text=_"Send test mailing" class="btn btn-default" title=_"Send this page to the test mailing list." action={mailing_page_test id=id} %}
        {% button text=_"Send test to address" class="btn btn-default" title=_"Send this page to a single address" action={dialog_open template="_dialog_mailing_testaddress.tpl" title=_"Send test to address" id=id} %}
            </div>

    </div>

    {# TODO: also reload if there is email activity for this mailinglist -- as stats table should be added #}
    <div id="mailing-status">
        {% live topic=["model", "mailinglist", "event", id, "+" ]
                template="_admin_mailing_status_overview.tpl"
                id=id
        %}
    </div>

    {% javascript %}
        document.getElementById("mailing-preview-btn").addEventListener("click",
            (e) => {
                window.open(e.target.getAttribute('href'), 'mailingpreview', 'width=800,height=800');
                e.preventDefault();
            });
    {% endjavascript %}

</div>
{% endblock %}
