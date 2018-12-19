{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Twitter Account _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-twitter{% endblock %}

{% block widget_content %}
<div class="form-group">
    <div>
        <input type="text" id="twitter-id" name="twitter_id" class="zp-100 form-control" value="{{ m.rsc[id].twitter_id }}" />
        <p class="help-block">
            {_ Use <tt>@foobar</tt> or <tt>foobar</tt> to import all recent and future tweets from "foobar". _}
            {_ Note that this user or organization must have <em>create</em> rights on the <em>Tweet</em> category. _}
        </p>
    </div>
</div>
{% endblock %}
