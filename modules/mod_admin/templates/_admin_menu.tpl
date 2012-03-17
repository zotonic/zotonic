
<div class="navbar navbar-fixed-top">

    <div class="navbar-inner">
        <div class="container">
            <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </a>
	    
            <a class="brand" href="http://{{ m.site.hostname }}" title="{_ visit site _}"><img src="/lib/images/admin_zotonic.png"></a>

            <div class="nav-collapse">
                <ul class="nav">
                    {% for id, item in m.admin_menu %}
                        {% if item.items %}
                        <li class="dropdown" id="nav-{{ id }}">
                            <a class="dropdown-toggle" data-toggle="dropdown" href="#nav-{{ id }}">
                                {{ item.label|escape }}
                                <b class="caret"></b>
                            </a>
                            <ul class="dropdown-menu">
                                {% for id, item in item.items %}
                                <li><a href="{{ item.url }}">{{ item.label|escape }}</a></li>
                                {% endfor %}
                            </ul>
                        </li>
                        {% else %}
                        <li class="">
                            <a href="{{ item.url }}">{{ item.label|escape }}</a>
                        </li>
                        {% endif %}
                    {% endfor %}
                </ul>
            </div>

	    {% block search %}
	    <div class="pull-right">
                {% all include "_admin_headeritem.tpl" %}
		<form class="navbar-form form-search" action="{% url admin_overview_rsc %}" method="get">
                    <input type="hidden" name="qsort" value="{{ q.qsort|escape }}" />
                    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
		    <input class="input-medium search-query" type="text" name="qs" value="{{q.qs|escape}}" placeholder="Search..." />
		</form>
	    </div>
	    {% endblock %}
            
        </div>
    </div>
</div>
