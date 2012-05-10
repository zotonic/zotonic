{# tablet+ footer template #}
<div class="row-fluid footer">
    <hr/>
    {% with m.rsc.menu_footer.menu as menu %}
    {% if menu %}
        <ul class="nav nav-footer">
        {% for pid,_m in menu %}
            <li><a {% if id == pid %}class="current"{% endif %} href="{{ pid.page_url }}">{{ pid.short_title|default:(pid.title) }}</a> <span class="divider">/</li>
        {% endfor %}
        </ul>
    {% endif %}
    {% endwith %}
    <span class="pull-right">&copy; {{ now|date:"Y" }} {{  m.config.site.title.value }}</span>
    {% include "_ua_select.tpl" dropup %}
</div>

