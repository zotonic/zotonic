{% extends "admin_base.tpl" %}

{% block title %}{_ Comment Form Settings _}{% endblock %}

{% block content %}

{% wire id="admin-comments-settings" type="submit" postback="admin_comments_settings" %}
<form name="admin-comments-settings" id="admin-comments-settings" method="POST" action="postback">

    <ul class="breadcrumb">
		<li><a href="{% url admin_comments %}">{_ Comments _}</a></li>
		<li class="active">{_ Settings _}</li>
    </ul>

    <div class="admin-header">
        <h2>{_ Comment Form Settings _}</h2>
        <p>{_ Here you find settings to configure the comment module to suit your needs. _}</p>
    </div>

    <div class="widget">
        <div class="widget-header">{_ General Comment Form Settings _}</div>
        <div class="widget-content">

            <div class="form-group">
                <div>
                    <label class="checkbox-inline" for="moderate" title="{_ Require all comments to be reviewed by a moderator before displaying them on the public website _}">
                        <input type="checkbox" id="moderate" name="moderate" value="1" {% if m.comment.moderate %}checked="checked"{% endif %} />
                        {_ Moderate comments _}
                    </label>
                </div>
            </div>

            <button class="btn btn-primary" type="submit">{_ Save Comment Form settings _}</button>

        </div>
    </div>

</form>


{% endblock %}
