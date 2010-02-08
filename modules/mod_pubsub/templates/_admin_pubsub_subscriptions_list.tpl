<h3 class="above-list">Subscriptions overview</h3>

<ul class="short-list">
    <li class="headers clearfix">
        <span class="zp-40">Title</span>
        <span class="zp-25">XMPP URI</span>
        <span class="zp-25">Location</span>
        <span class="zp-10">Options</span>
    </li>
    
    {% with m.search.paged[{query authoritative=0 page=q.page}] as result %}
    
    {% for id in result %}
    <li id="{{ #li.id }}" {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
        <a href="{% url admin_edit_rsc id=id %}" class="clearfix">
            <span class="zp-40">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
            <span class="zp-25">{{ m.rsc[id].pubsub_xmpp_uri|default:"&nbsp;" }}</span>
            <span class="zp-25">{{ m.rsc[id].uri|default:"&nbsp;" }}</span>
            
            <span class="zp-10">
                {% button text="view" action={redirect id=id} %}
                {% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
            </span>
        </a>
    </li>
    {% empty %}
    <li>
        No subscriptions found.
    </li>
    {% endfor %}
</ul>

{% pager result=result qargs %}

{% endwith %}
