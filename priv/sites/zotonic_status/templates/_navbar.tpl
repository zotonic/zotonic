<div class="navbar navbar-default navbar-fixed-top">

    <div class="container">
        <div class="container">
	    
            <a class="navbar-brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><img alt="zotonic logo" src="/lib/images/zotonic_gray.png" width="106" height="20"></a>

            <div class="navbar-collapse pull-right">
                <ul class="nav navbar-nav">

                    {% if m.acl.user %}
	            <li{% if zotonic_dispatch == "home" %} class="active"{% endif %}>
                        <a href="{% url home %}">{_ Sites _}</a>
                    </li>

	            <li><a id="{{ #logoff }}" href="#logoff">{_ Log Off _}</a>
                    </li>
	            {% wire id=#logoff postback={logoff} %}
                    {% endif %}
                </ul>
            </div>

        </div>
    </div>
</div>
