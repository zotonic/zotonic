{% extends "admin_base.tpl" %}

{% block title %}{_ Facebook API Configuration _}{% endblock %}

{% block content %}

<div id="content" class="zp-85">
  <div class="block clearfix">

    {% wire id="admin_facebook" type="submit" postback="admin_facebook" %}
    <form name="admin_facebook" id="admin_facebook" method="POST" action="postback">

    <h2>{_ Facebook API Configuration _}</h2>
    <p>{_ Here you find the settings to authenticate users with facebook. You can find these values in <a href="http://www.facebook.com/developers/apps.php" title="Developer Dashboard">Your Facebook Developer Dashboard</a> _}</p>

    <div class="item-wrapper">
      <h3 class="above-item">Facebook</h3>
      <div class="item">
        <fieldset class="admin-form">
          <div class="form-item clearfix">
            <label for="app_id">{_ App ID. _}
            </label>
            <input type="text" id="appid" name="appid" value="{{ m.config.mod_facebook.appid.value|escape }}" />
          </div>
          <div class="form-item clearfix">
            <label for="appsecret">{_ App Secret _}</label>
            <input type="text" id="appsecret" name="appsecret" value="{{ m.config.mod_facebook.appsecret.value|escape }}" />
          </div>
	  <div class="form-item clearfix">
            <label for="scope">{_ Scope _}</label>
            <input type="text" id="scope" name="scope" value="{{ m.config.mod_facebook.scope.value|default:'email'|escape }}" />
          </div>
        </fieldset>
      </div>
    </div>

    <div class="form-item clearfix">
      <button type="submit">{_ Save Facebook settings _}</button>
    </div>

    </form>
  </div>
</div>


{% endblock %}
