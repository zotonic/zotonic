{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Twitter ID _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{_ Finding a Twitter ID. _}', text: '{_ The (numeric) Twitter ID of a user can be found on the twitter page of the user: it is the number that is contained in the URL to the RSS feed for the user\'s updates. _}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-twitter{% endblock %}

{% block widget_content %}
<div class="form-group">
    <div>
        <input type="text" id="twitter-id" name="twitter_id" class="zp-100 form-control" value="{{ m.rsc[id].twitter_id }}" />
        <p class="help-block">
            {_ Fill in the <strong>numeric</strong> Twitter id. To map a Twitter account name to this number go to: _}
            <a target="_blank" href="http://www.idfromuser.com/">www.idfromuser.com</a>.
        </p>
    </div>
</div>

{% endblock %}
