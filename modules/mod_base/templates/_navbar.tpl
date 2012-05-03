<div class="navbar">
    <div class="navbar-inner">
        <div class="container">
            <a class="brand" href="/">{{ m.config.site.title.value }} {% if m.config.site.subtitle.value %}{% endif %}</a>
            
            <div class="pull-right">
                {% menu id=id %}
            </div>
        </div>
    </div>
</div><!-- end navbar -->
