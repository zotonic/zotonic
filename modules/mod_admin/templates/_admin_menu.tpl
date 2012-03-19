
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
                                {% if item.icon %}<i class="icon-{{ item.icon }} icon-white"></i>{% endif %}
                                {{ item.label|escape }}
                                <b class="caret"></b>
                            </a>
                            <ul class="dropdown-menu">
                                {% for id, item in item.items %}
                                {% if item.separator %}
                                <li class="divider"></li>
                                {% else %}
                                <li><a href="{{ item.url }}">
                                    {% if item.icon %}<i class="icon-{{ item.icon }}"></i>{% endif %}
                                    {{ item.label|escape }}</a>
                                </li>
                                {% endif %}
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
                <ul class="nav">
                    {% all include "_admin_headeritem.tpl" %}
                    <li>
		        <form class="navbar-form form-search" action="{% block search_target %}{% url admin_overview_rsc %}{% endblock %}" method="get">
                            <input type="hidden" name="qsort" value="{{ q.qsort|escape }}" />
                            <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
		            <input class="input-medium search-query" type="text" name="qs" value="{{q.qs|escape}}" placeholder="Search..." />
		        </form>
                    </li>
                </ul>
	    </div>
	    {% endblock %}
            
        </div>
    </div>
</div>
