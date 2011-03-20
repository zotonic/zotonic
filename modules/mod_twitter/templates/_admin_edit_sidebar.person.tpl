{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Twitter ID _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}
<div class="form ">
	<div class="form-item clearfix">

                <input type="text" id="twitter-id" name="twitter_id" class="zp-100" value="{{ m.rsc[id].twitter_id }}" />
                <a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Finding a Twitter ID. _}', text: '{_ The (numeric) Twitter ID of a user can be found on the twitter page of the user: it is the number that is contained in the URL to the RSS feed for the user\'s updates. _}', width: '450px'">{_ Need more help? _}</a>

        </div>
</div>
{% endblock %}
