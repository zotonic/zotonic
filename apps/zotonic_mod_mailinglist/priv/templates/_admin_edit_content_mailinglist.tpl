{% extends "admin_edit_widget_i18n.tpl" %}

{# To edit the stored search query #}

{% block widget_title %}
{_ Mailinglist email text and recipients query _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Help about mailing lists",
                text: _"<h3>Recipients</h3><p>To add, remove or view the mailing list recipients, click on the “show all recipients” link.</p><h3>Sender name and e-mail address</h3><p>The sender name and e-mail address can be set per mailing list. This defaults to the config key <tt>site.email_from</tt>.  The <i>From</i> of the sent e-mails will be set to the sender name and address.</p><h3>Automatic upload of recipient lists</h3><p>The drop folder filename is used for the automatic upload of complete recipients list. The filename must match the filename of the uploaded recipient list. The complete list of recipients will be replaced with the recipients in the drop folder file.</p><h3>Access control</h3><p>Everybody who can edit a mailing list is also allowed to send a page to the mailing list. Everybody who can view the mailing list is allowed to add an e-mail address to the mailing list.</p>"
            }|escape
        }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}

{% block widget_content_nolang_before %}
    <div class="row">
        <div class="col-sm-6">
            <div class="form-group label-floating">
                <input class="form-control" id="{{ #sender }}" type="text" name="mailinglist_sender_name" value="{{ id.mailinglist_sender_name }}" placeholder="{_ Sender name for e-mails (optional) _}">
                <label class="control-label" for="{{ #sender }}">{_ Sender name for e-mails (optional) _}</label>
            </div>

            <div class="form-group label-floating">
                <input class="form-control" type="text" id="mailinglist_reply_to" name="mailinglist_reply_to" value="{{ id.mailinglist_reply_to }}" placeholder="{_ Sender address for e-mails (optional) _}">
                {% validate id="mailinglist_reply_to" type={email} %}
                <label class="control-label">{_ Sender address for e-mails (optional) _}</label>
            </div>
        </div>
        <div class="col-sm-6">
            <div class="form-group label-floating">
                <input class="form-control" id="{{ #dropbox }}" type="text" name="mailinglist_dropbox_filename" value="{{ id.mailinglist_dropbox_filename }}"
                placeholder="{_ Drop folder filename (optional) _}">
                <label for="{{ #dropbox }}" class="control-label">{_ Drop folder filename (optional) _}</label>
                <p class="help-block">{_ Used for uploading CSV files with recipients. _}</p>
            </div>

            <div class="form-group">
                <label class="checkbox">
                    <input type="checkbox" type="email" id="mailinglist_private" name="mailinglist_private" value="1" {% if id.mailinglist_private %}checked="checked"{% endif %}>
                    {_ Externally managed list &mdash; no unsubscribe/subscribe links _}
                </label>
            </div>
        </div>
    </div>

    <p>{_ Mailings are sent to: _}</p>
    <ul>
        <li>{_ Subscribers, added via the subscription form. _}</li>
        <li>{_ Pages that have the 'subscriberof' edge to the mailinglist. _}</li>
        <li>{_ Pages that match the recipients search query below. _}</li>
    </ul>
    <p>
         <a class="btn btn-default" href="{% url admin_mailinglist_recipients id=id %}">{_ Show all recipients _} &raquo;</a>
    </p>
{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <p class="help-block">{_ Text shown below the confirmation, welcome and unsubscribe e-mails. _}</p>

        {% with is_i18n|if
            :id.translation[lang_code]['subscription_info_html']
            :id.subscription_info_html as text
        %}
            {% if not id or id.is_editable %}
                <textarea rows="10" cols="10" id="rsc-subscription_info_html{{ lang_code_for_id }}" name="subscription_info_html{{ lang_code_with_dollar }}" class="body z_editor-init form-control" {% include "_language_attrs.tpl" language=lang_code %}>{{ text|escape }}</textarea>
            {% else %}
                {{ text }}
            {% endif %}
        {% endwith %}
    </div>
{% endblock %}

{% block widget_content_nolang %}
    <div class="form-group">
        <details {% if id.query %}open{% endif %}>
            <summary>{_ Recipients search query _}</summary>

            <p class="help-block">
                {_ Here you can edit the arguments of the search query. Every argument goes on its own line. For more information, see the <a href="https://docs.zotonic.com/en/latest/developer-guide/search.html#query-arguments">documentation on the query arguments</a> on the Zotonic website. _}
            </p>

            <div class="form-group">
                {% with [ _"Query, for example:", "\n\ncat='person'" ] as placeholder %}
                <textarea class="form-control" id="{{ #query }}" name="query" rows="10" placeholder="{{ placeholder }}">{{ id.query }}</textarea>
                {% endwith %}
                {% wire id=#query type="change" postback={query_preview rsc_id=id div_id=#querypreview target_id=#query} delegate="controller_admin_edit" %}
            </div>
            <div class="form-group">
                <a id="{{ #test_query }}" class="btn btn-default">{_ Test query _}</a>
                {% wire id=#test_query type="click" action={script script="$('#query').trigger('change')"} %}
            </div>

            <h4>{_ Query preview _}</h4>

            <div class="query-results" id="{{ #querypreview }}">
                {% if id.query %}
                    {% include "_admin_query_preview.tpl" result=m.search[{query query_id=id pagelen=20}] %}
                {% else %}
                    <p class="text-muted">{_ No results. _}</p>
                {% endif %}
            </div>
        </details>
    </div>
{% endblock %}
