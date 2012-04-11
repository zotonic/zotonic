{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Twitter ID _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-twitter{% endblock %}

{% block widget_content %}

<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{_ Finding a Twitter ID. _}', text: '{_ The (numeric) Twitter ID of a user can be found on the twitter page of the user: it is the number that is contained in the URL to the RSS feed for the user\'s updates. _}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>

<div class="control-group">
    <div class="controls">
        <input type="text" id="twitter-id" name="twitter_id" class="zp-100" value="{{ m.rsc[id].twitter_id }}" />
    </div>
</div>

{% endblock %}
