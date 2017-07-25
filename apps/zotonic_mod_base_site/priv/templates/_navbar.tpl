{# navbar for phone+ #}
<nav class="navbar navbar-inverse navbar-fixed-top">
    <div class="container">
        <div class="navbar-header">
            <!-- .btn-navbar is used as the toggle for collapsed navbar content -->
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target=".navbar-collapse">
                <span class="sr-only">{_ Toggle navigation _}</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>

            <a class="navbar-brand" href="/">
            {% block navbar_brand %}
                <span class="zotonic-logo"><em>Zotonic</em></span>
                {{ m.config.site.title.value }}
            {% endblock %}
            </a>
        </div>

        <div class="navbar-collapse collapse">
            <ul class="nav navbar-nav navbar-right">
                {% include "_language_switch.tpl" is_nav %}
            </ul>
            <form class="navbar-form navbar-right" method="get" action="{% url search %}">
                <input type="text" class="search-query form-control" placeholder="Search" name="qs"/>
            </form>
			{% menu menu_id=menu_id id=id maxdepth=2 %}
        </div>
    </div>
</nav>
