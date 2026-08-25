{% extends "admin_base.tpl" %}

{% block title %}{% trans "Send “{title}” to a mailing list" title=m.rsc[id].title %}{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_mailinglist %}">{_ Mailing lists _}</a></li>
    <li class="active">{_ Send page _}</li>
</ul>

<div class="admin-header">
    <h2>{% trans "Send “{title}” to a mailing list" title=m.rsc[id].title %}</h2>

    <p>{_ Select a mailing list below, or preview and test the mailing first. _} <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Help with sending a page",
                text: _"Use this overview to send the current page as an email to the recipients of a mailing list. You can preview the mailing or send a test before selecting a mailing list below. Test mailings can be sent while the page is unpublished. The table shows the status and statistics for each mailing list."
            }|escape
        }}" title="{_ Need more help? _}"></a>
    </p>
    <div class="well">
        <a class="btn btn-primary" href="{% url admin_edit_rsc id=id %}">{_ Edit page _}</a>
        <a class="btn btn-default" href="{% url admin_mailing_preview id=id %}" id="mailing-preview-btn">{_ Preview mailing _}</a>
        {% button text=_"Send test mailing now"
                  class="btn btn-default"
                  title=_"Send this page immediately to the test mailing list. The page does not need to be published."
                  action={mailing_page_test id=id}
        %}
        {% button text=_"Send test to address"
                  class="btn btn-default"
                  title=_"Send this page to a single address"
                  action={dialog_open
                    template="_dialog_mailing_testaddress.tpl"
                    title=_"Send test to address"
                    id=id
                  }
        %}
    </div>

    <p class="help-block">
        {_ Test mailings are sent immediately. The page does not need to be published, but you must have permission to view it. _}
    </p>

</div>

{# TODO: also reload if there is email activity for this mailinglist -- as stats table should be added #}
<div id="mailing-status">
    {% live topic=["bridge", "origin", "model", "mailinglist", "event", id, "+" ]
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

{% endblock %}
