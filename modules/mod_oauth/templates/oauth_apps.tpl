{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth applications _}{% endblock %}

{% block content %}
<style type="text/css">
    #oauth-apps li {
    margin-top: 10px;
    }

    #oauth-apps li dl {
    padding: 4px; background-color: #dddddd;
    }
    #oauth-apps li dl dt {
    float: left; width: 120px;
    clear: left;
    }
    #oauth-apps li dl dd {
    float: left; width: 400px;
    font-family: courier;
    }
    #oauth-add {
    display: none;
    }
</style>

<div id="content" class="zp-85">
    <div class="block clearfix">
        
		<h2>{_ Registered OAuth applications _}</h2>
        <p>
            {_ This page allows you to register API keys with which 3rd parties can gain access to specific parts of the API and database. _}
        </p>

        <ul id="oauth-apps">
            {% include "_oauth_apps_list.tpl" %}
        </ul>
        
        <p class="clearfix">
            {% button text="Add new application" postback="start_add_app" %}
        </p>
        
    </div>
</div>

{% endblock %}

