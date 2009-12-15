<p class="article-meta">
    Posted on {{ m.rsc[id].publication_start|date:"d F Y"}}, 
    {% if m.rsc[id].author %}
    by {{ m.rsc[id].author.title }},
    {% endif %}
    {{ m.rsc[id].publication_start|timesince }}.
</p>
