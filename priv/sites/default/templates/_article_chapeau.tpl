{% with id|menu_trail as parents %}
{% if parents %}
<h5 class="chapeau">
    <a href="{{ m.rsc['page_home'].page_url }}">Home</a> 
    {% for p in parents %}
    &raquo;
    <a href="{{ m.rsc[p].page_url }}">{{ m.rsc[p].title }}</a>
    {% endfor %}
</h5>
{% endif %}
{% endwith %}
