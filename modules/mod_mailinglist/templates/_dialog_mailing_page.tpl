{% with m.mailinglist.rsc_stats[id] as rsc_stats %}

<p>{_ Please confirm that you want to send the page _} {{ m.rsc[id].title }} {_ to the list _} {{ m.rsc[list_id].title }}</p>

{% if rsc_stats[list_id].total > 0 %}
<p><strong>{_ Please note: _}</strong> {_ It appears you have sent
    this page once already to this list. If you send it again, only
    the recipients that did not yet receive the mail will get it. As a
    safety-caution, it is impossible to send the same page twice to
    the same e-mail address. _}</p>
{% endif %}


{% wire type="submit" id=#form postback={mailing_page id=id on_success=on_success} action={dialog_close} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">
    <input type="hidden" name="list_id" value="{{ list_id }}" />

    {% if not m.rsc[id].is_published or m.rsc[id].publication_start|in_future %}
    <p>{_ The page you are trying to e-mail is not yet published. What do you want to do? _}</p>
    
    <p><input type="radio" name="mail_when" value="now" checked="checked" /> {_ Send the mailing right now, but do not include a link back to the website. _}<br />
        <input type="radio" name="mail_when" value="scheduled" />
        {% if not m.rsc[id].is_published %}
        {_ Send the mailing immediately after the "published" checkbox has been checked in the edit page. _}
        {% else %}
        {_ Send the mailing automatically after the publication start date of _} {{ m.rsc[id].publication_start|date:"d M Y, H:i" }}.
        {% endif %}
    </p>
	{% endif %}

    <div class="modal-footer">
	    {% button class="btn" text=_"Cancel" action={dialog_close} tag="a" %}
	    {% button class="btn btn-primary" text=_"Send mailing" %}
    </div>
</form>

{% endwith %}
