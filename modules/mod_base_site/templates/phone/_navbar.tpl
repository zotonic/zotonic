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
                <span class="zotonic-logo"></span>
                {{ m.config.site.title.value }}
            </a>
        </div>
        
        <div class="navbar-collapse collapse">
            <form class="navbar-form pull-right" method="get" action="{% url search %}">
                <input type="text" class="search-query form-control" placeholder="Search" name="qs"/>
            </form>
			<div class="navbar-right">
                {% include "_navbar_right.tpl"%}
            </div>        
			{% menu menu_id=menu_id id=id maxdepth=2 %}
        </div>
    </div>
</div>
</nav>
