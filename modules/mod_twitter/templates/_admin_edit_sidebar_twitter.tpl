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
                {_ All tweets from this account will be automatically imported. _}
            </p>
        </div>
    </div>
{% endblock %}
