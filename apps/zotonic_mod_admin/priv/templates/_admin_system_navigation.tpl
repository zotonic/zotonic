<nav aria-label="{_ System administration _}">
    <ul class="nav nav-tabs">
        <li {% if zotonic_dispatch == `admin_status` %}class="active"{% endif %}><a href="{% url admin_status %}" {% if zotonic_dispatch == `admin_status` %}aria-current="page"{% endif %}>{_ Status _}</a></li>
        {% if m.acl.use.mod_admin_config %}
        <li {% if zotonic_dispatch == `admin_maintenance` %}class="active"{% endif %}><a href="{% url admin_maintenance %}" {% if zotonic_dispatch == `admin_maintenance` %}aria-current="page"{% endif %}>{_ Maintenance _}</a></li>
        <li {% if zotonic_dispatch == `admin_system_info` %}class="active"{% endif %}><a href="{% url admin_system_info %}" {% if zotonic_dispatch == `admin_system_info` %}aria-current="page"{% endif %}>{_ System information _}</a></li>
        {% endif %}
    </ul>
</nav>
