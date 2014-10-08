
<div class="navbar navbar-default navbar-fixed-top" role="navigation">
    <div class="">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target=".navbar-collapse">
                <span class="sr-only">{_ Toggle navigation _}</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>

            <a class="navbar-brand" href="/" title="{_ visit site _}">
                <span class="zotonic-logo"></span>
            </a>
        </div>
        
        <div class="navbar-collapse collapse">

            <ul class="nav navbar-nav">
                {% for id, item in m.admin_menu %}
                    {% if item.items %}
                        <li class="dropdown" id="nav-{{ id }}">
                            <a class="dropdown-toggle" data-toggle="dropdown" href="#nav-{{ id }}">
                                {% if item.icon %}<i class="glyphicon glyphicon-{{ item.icon }}"></i>{% endif %}
                                {{ item.label|escape }}
                                <b class="caret"></b>
                            </a>
                            <ul class="dropdown-menu">
                                {% for id, item in item.items %}
                                    {% if item.separator %}
                                        <li class="divider"></li>
                                    {% else %}
                                        <li><a href="{{ item.url }}">
                                                {% if item.icon %}<i class="glyphicon glyphicon-{{ item.icon }}"></i>{% endif %}
                                                {{ item.label|escape }}</a>
                                        </li>
                                    {% endif %}
                                {% endfor %}
                            </ul>
                        </li>
                    {% else %}
                        <li>
                            <a href="{{ item.url }}">{{ item.label|escape }}</a>
                        </li>
                    {% endif %}
                {% endfor %}
                <li>
                    <a href="#" id="{{ #logoff }}" title="{_ Log Off _}"><i class="glyphicon glyphicon-off"></i></a>
                    {% wire id=#logoff action={confirm title=_"Confirm logoff" text=_"Are you sure you want to exit the admin interface?"
                            action={redirect dispatch=`logoff`}} %}
                </li>
            </ul>

            <div class="navbar-right">
                <ul class="nav navbar-nav">
                    {% all include "_admin_headeritem.tpl" %}
                </ul>
            </div>
            
            {% block search %}
                <form class="navbar-right navbar-form form-inline hidden-sm" action="{% block search_target %}{% url admin_overview_rsc %}{% endblock %}" method="get">
                    <input type="hidden" name="qsort" value="{{ q.qsort|escape }}" />
                    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
                    <input class="search-query col-md-6 form-control" type="text" name="qs" value="{{q.qs|escape}}" placeholder="Search..." />
                </form>
            {% endblock %}
        </div>
    </div>
</div>
