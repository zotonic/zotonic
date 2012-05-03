{% extends "admin_base.tpl" %}

{% block title %}{_ Comment Form Settings _}{% endblock %}

{% block content %}

{% wire id="admin-comments-settings" type="submit" postback="admin_comments_settings" %}
<form name="admin-comments-settings" id="admin-comments-settings" method="POST" action="postback">

<div class="admin-header">

    <h2>{_ Comment Form Settings _}</h2>

    <p>{_ Here you find settings to configure the comment module to suit your needs. _}</p>

    <div class="well">
        <button class="btn btn-primary" type="submit">{_ Save Comment Form settings _}</button>
    </div>
</div>


    <div class="widget">

        <h3 class="widget-header">{_ General Comment Form Settings _}</h3>
        <div class="widget-content">

            <div class="control-group">
                <div class="controls">
                    <label class="inline checkbox" for="comments-moderate" title="{_ Require all comments to be reviewed by a moderator before displaying them on the public website _}">
                        <input type="checkbox" id="comments-moderate" name="comments-moderate" value="1" {% if m.config.comments.moderate.value %}checked="checked"{% endif %} />
                        {_ Moderate comments _}
                    </label>
                </div>
            </div>

        </div>
    </div>
    
</form>


{% endblock %}
