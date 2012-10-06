{% extends "admin_base.tpl" %}

{% block title %}{_ Facebook API Configuration _}{% endblock %}

{% block content %}

<div class="admin-header">

    <h2>{_ Facebook API Configuration _}</h2>

    <p>{_ Here you find the settings to authenticate users with facebook. You can find these values in <a href="http://www.facebook.com/developers/apps.php" title="Developer Dashboard">Your Facebook Developer Dashboard</a> _}</p>

    {% wire id="admin_facebook" type="submit" postback="admin_facebook" %}
    <form name="admin_facebook" id="admin_facebook" method="POST" action="postback">

        <div class="widget">
            <h3 class="widget-header">Facebook</h3>
            <div class="widget-content">

                <div class="control-group">
                    <label for="app_id">{_ App ID. _}</label>
                    <div class="controls">
                        <input type="text" id="appid" name="appid" value="{{ m.config.mod_facebook.appid.value|escape }}" class="span6" />
                    </div>
                </div>
                
                <div class="control-group">
                    <label class="control-label" for="appsecret">{_ App Secret _}</label>
                    <div class="controls">
                        <input type="text" id="appsecret" name="appsecret" value="{{ m.config.mod_facebook.appsecret.value|escape }}" class="span6" />
                    </div>
                </div>
                
                <div class="control-group">
                    <label class="control-label" for="scope">{_ Scope _}</label>
                    <div class="controls">
                        <input type="text" id="scope" name="scope" value="{{ m.config.mod_facebook.scope.value|default:'email'|escape }}" class="span6" />
                    </div>
                </div>

                <div class="control-group">
                    <div class="controls">
                        <label class="checkbox" for="useauth">
                            <input type="checkbox" id="useauth" name="useauth" {% if m.config.mod_facebook.useauth.value %}checked="checked"{% endif %} value="1" />
                            {_ Use Facebook authentication _}
                        </label>
                    </div>
                </div>

                <div class="control-group">
                    <div class="controls">
                        <button class="btn btn-primary" type="submit">{_ Save Facebook Settings _}</button>
                    </div>
                </div>
                
            </div>
        </div>
        
    </form>
</div>


{% endblock %}
