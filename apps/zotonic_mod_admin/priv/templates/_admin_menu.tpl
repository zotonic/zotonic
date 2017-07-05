
<div class="navbar navbar-branded navbar-fixed-top" role="navigation">
    <div class="">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target=".navbar-collapse">
                <span class="sr-only">{_ Toggle navigation _}</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>

            {% include "_admin_navbar_brand.tpl" %}
        </div>

        <div class="navbar-collapse collapse" id="admin_menu_collapse">

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
            </ul>

            <div class="navbar-right">
                <ul class="nav navbar-nav">
                    {% all include "_admin_headeritem.tpl" is_nav %}

                    {# at the far right is the account menu #}
                    {% with
                        "account"
                        as
                        id
                    %}
                        <li class="dropdown" id="nav-{{ id }}">
                            <a class="dropdown-toggle has-icon" data-toggle="dropdown" href="#nav-{{ id }}">
                                <i class="z-icon z-icon-user"></i>
                                <span class="z-username">{{ m.acl.user.title|escape|truncate_html:20 }}</span>
                                <b class="caret"></b>
                            </a>
                            <ul class="dropdown-menu">
                                <li class="dropdown-header z-username">
                                    {{ m.acl.user.title|escape }}
                                </li>
                                <li>
                                    <a href="{% url admin_edit_rsc id=m.acl.user %}">{_ User page _}</a>
                                </li>
                                <li>
                                    <a href="#" id="{{ #logoff }}">{_ Log Off _}</a>
                                    {% wire id=#logoff action={confirm title=_"Confirm logoff" text=_"Are you sure you want to exit the admin interface?"
                                            action={redirect dispatch=`logoff`}} %}
                                </li>
                            </ul>
                        </li>
                    {% endwith %}
                </ul>
            </div>

            {% block search %}
                <form class="navbar-right navbar-form form-inline" action="{% block search_target %}{% url admin_overview_rsc %}{% endblock %}" method="get">
                    <input type="hidden" name="qsort" value="{{ q.qsort|escape }}" />
                    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
                    <input type="hidden" name="qquery" value="{{ q.qquery|escape }}" />
                    <input class="search-query form-control" type="text" name="qs" value="{{q.qs|escape}}" placeholder="{% block search_placeholder %}{_ Search... _}{% endblock %}" />
                </form>
            {% endblock %}

        </div>
    </div>
</div>

{% javascript %}
$('#admin_menu_collapse').on('show.bs.collapse', function () {
    $("body").addClass("navbar-menu-open");
});
$('#admin_menu_collapse').on('hide.bs.collapse', function () {
    $("body").removeClass("navbar-menu-open");
});
{% endjavascript %}
