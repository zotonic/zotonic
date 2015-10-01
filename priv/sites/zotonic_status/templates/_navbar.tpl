<div class="navbar" role="navigation">
    <div class="container-fluid">
        <!-- Brand and toggle get grouped for better mobile display -->
        <div class="navbar-header">
            {% if m.acl.user %}
                <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
            {% endif %}
            <a class="navbar-brand" href="/">
                <span class="zotonic-logo"><em>Zotonic</em></span>
                {_ Status _}
            </a>
        </div><!-- /.navbar-header -->
        
        {% if m.acl.user %}
            <!-- Collect the nav links, forms, and other content for toggling -->
            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                <ul class="nav navbar-nav navbar-right">
{#
Add menu item when we have more top bar items
                    <li{% if zotonic_dispatch == "home" %} class="active"{% endif %}>
                        <a href="{% url home %}">{_ Sites _}</a>
                    </li>
#}
                    <li>
                        <a id="logoff" href="#" title="{_ Log Off _}">
                            <span class="z-icon z-icon-off"></span>
                        </a>
                        {% wire id="logoff" postback={logoff} %}
                    </li>
                </ul>
            </div><!-- /.navbar-collapse -->
        {% endif %}
    </div>
</div>
