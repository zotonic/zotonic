<div class="navbar navbar-fixed-top">

    <div class="navbar-inner">
        <div class="container">
	    
            <a class="brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><img alt="zotonic logo" src="/lib/images/zotonic.png" width="106" height="20"></a>

            <div class="nav-collapse pull-right">
                <ul class="nav">

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
