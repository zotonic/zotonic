{% cache 3600 cat='article' vary=id %}

<div class="prevnext">

    {% for id in m.search[{next cat='article' id=id pagelen=1}] %}
	    <div class="next">
    	    <a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Next post &raquo;</a>
	    </div>
    {% endfor %}

    {% for id in m.search[{previous cat='article' id=id pagelen=1}] %}
    	<div class="prev">
        	<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">&laquo; Previous post</a>
	    </div>
    {% endfor %}

</div>

{% endcache %}