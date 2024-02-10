
<nav class="navbar navbar-branded fixed-top navbar-expand-lg" role="navigation">
    <div class="container-fluid">
        {% include "_admin_navbar_brand.tpl" %}

        <button 
            class="navbar-toggler" 
            type="button" 
            data-bs-toggle="collapse" 
            data-bs-target="#navbarSupportedContent" 
            aria-controls="navbarSupportedContent" 
            aria-expanded="false" 
            aria-label="Toggle navigation">
            <span class="navbar-toggler-icon"></span>
        </button>

        <div class="collapse navbar-collapse" id="navbarSupportedContent">
            <ul class="navbar-nav me-auto mb-2 mb-lg-0">
                {% for id, item in m.admin_menu %}
                    {% if item.items %}
                        <li class="nav-item dropdown" id="nav-{{ id }}">
                            <a class="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">
                                {% if item.icon %}<i class="{{ item.icon }}"></i>{% endif %}
                                {{ item.label|escape }}
                                <b class="caret"></b>
                            </a>

                            <ul class="dropdown-menu">
                                {% for id, item in item.items %}
                                    {% if item.separator %}
                                        <li><hr class="dropdown-divider"></li>
                                    {% else %}
                                        <li>
                                            <a href="{{ item.url }}" class="dropdown-item">
                                                {% if item.icon %}<i class="{{ item.icon }}"></i>{% endif %}
                                                {{ item.label|escape }}
                                            </a>
                                        </li>
                                    {% endif %}
                                {% endfor %}
                            </ul>
                        </li>
                    {% else %}
                        <li class="nav-item">
                            <a href="{{ item.url }}" class="nav-link">{{ item.label|escape }}</a>
                        </li>
                    {% endif %}
                {% endfor %}
            </ul>
            
            {% block search %}
                <form class="navbar-form d-flex" action="{% block search_target %}{% url admin_overview_rsc %}{% endblock %}" method="get">
                    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
                    <input type="hidden" name="qquery_id" value="{{ q.qquery_id|escape }}" />
                    
                    <label class="visually-hidden">{_ Search _}</label>

                    <input autocomplete="off" class="search-query form-control me-2" type="search" name="qs" value="{{q.qs|escape}}" placeholder="{% block search_placeholder %}{_ Search... _}{% endblock %}" data-search-view="true" />
                </form>
            {% endblock %}

            <ul class="nav navbar-nav">
                {% all include "_admin_headeritem.tpl" is_nav %}

                {# at the far right is the account menu #}
                {% with
                    "account"
                    as
                    id
                %}
                    <li class="nav-item dropdown" id="nav-{{ id }}">                        
                        <a href="#nav-{{ id }}" id="navbarDropdown-{{ id }}" role="button" class="nav-link dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false">
                            <i class="fa-solid fa-user"></i>
                            <span>{{ m.acl.user.title|escape|truncate_html:20 }}</span>
                            <b class="caret"></b>
                        </a>
                        <ul class="dropdown-menu" aria-labelledby="navbarDropdown-{{ id }}">
                            <li class="dropdown-header z-username">
                                {{ m.acl.user.title|escape }}
                            </li>
                            <li>
                                <a href="{% url admin_edit_rsc id=m.acl.user %}" class="dropdown-item">{_ User page _}</a>
                            </li>
                            <li>
                                <a href="#" id="{{ #logoff }}" class="dropdown-item">{_ Log Off _}</a>
                                {% wire id=#logoff action={confirm title=_"Confirm logoff" text=_"Are you sure you want to exit the admin interface?"
                                        action={redirect dispatch=`logoff`}} %}
                            </li>
                        </ul>
                    </li>
                {% endwith %}
            </ul>
        </div>
    </div>
</nav>

{# {% javascript %}
$('#admin_menu_collapse').on('show.bs.collapse', function () {
    $("body").addClass("navbar-menu-open");
});
$('#admin_menu_collapse').on('hide.bs.collapse', function () {
    $("body").removeClass("navbar-menu-open");
});
{% endjavascript %}
 #}