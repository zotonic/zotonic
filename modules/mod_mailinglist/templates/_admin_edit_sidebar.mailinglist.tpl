{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Mailing list _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<div class="notification notice">
	{_ Mailing list settings. _}
	<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about mailing lists. _}', text: '{_ <h3>Recipients</h3><p>To add, remove or view the mailing list recipients, click on the “show all recipients” link.</p><h3>Sender name and e-mail address</h3><p>The sender name and e-mail address can be set per mailing list. This defaults to the config key <tt>site.email_from</tt>.  The <i>From</i> of the sent e-mails will be set to the sender name and address.</p><h3>Automatic upload of recipient lists</h3><p>The dropbox filename is used for the automatic upload of complete recipients list. The filename must match the filename of the uploaded recipient list. The complete list of recipients will be replaced with the recipients in the dropbox file.</p><h3>Access control</h3><p>Everybody who can edit a mailing list is also allowed to send a page to the mailing list. Everybody who can view the mailing list is allowed to add an e-mail address to the mailing list.</p> _}', width: '450px'">{_ Need more help? _}</a>
</div>
<p><a href="{% url admin_mailinglist_recipients id=id %}">{_ Show all recipients &raquo; _}</a></p>
<fieldset class="admin-form">
	<div class="form-item clear">
		<label>{_ Sender name for e-mails (optional) _}</label>
		<input type="text" name="mailinglist_sender_name" value="{{ r.mailinglist_sender_name }}" />
	</div>

	<div class="form-item clear">
		<label>{_ Sender address for e-mails (optional) _}</label>
		<input type="text" id="mailinglist_reply_to" name="mailinglist_reply_to" value="{{ r.mailinglist_reply_to }}" />
		{% validate id="mailinglist_reply_to" type={email} %}
	</div>

    <div class="form-item clearfix">
        <input type="checkbox" class="do_fieldreplace" id="mailinglist_private" name="mailinglist_private" value="1" {% if r.mailinglist_private %}checked="checked"{% endif %}/>
        <label for="mailinglist_private" class="left">{_ Externally managed list &mdash; no (un)subscribe links _}</label>
    </div>

	<hr/>

	<div class="form-item clear">
		<label>{_ Dropbox filename (optional) _}</label>
		<input type="text" name="mailinglist_dropbox_filename" value="{{ r.mailinglist_dropbox_filename }}" />
	</div>
</fieldset>
{% endwith %}
{% endblock %}
