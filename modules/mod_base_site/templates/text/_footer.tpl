{# text footer template #}
<div class="footer">
    <table>
    {% with m.rsc.menu_footer.menu as menu %}
    {% if menu %}
    <tr>
        <td>
        <ul class="nav nav-footer">
        {% for pid,_m in menu %}
            <li><a {% if id == pid %}class="current"{% endif %} href="{{ pid.page_url }}">{{ pid.short_title|default:(pid.title) }}</a> <span class="divider">/</li>
        {% endfor %}
        </ul>
        </td>
    </tr>
    {% endif %}
    {% endwith %}
    <tr>
        <td>{% include "_ua_select.tpl" dropup %}</td>
    </tr>
    <tr class="copyright">
        <td>&copy; {{ now|date:"Y" }} {{  m.config.site.title.value }}</td>
    </tr>
    </table>
</div>
