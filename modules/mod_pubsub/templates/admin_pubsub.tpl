{% extends "admin_base.tpl" %}

{% block title %} Publish/subscribe {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Publish/subscribe</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text="subscribe to a page" action={dialog_pubsub_subscribe title=""} %}
			</div>

			<hr />
			{% endif %}

			<p>This list shows on which pages this website is <em>subscribed</em>: for which items in the site it will receive updates from other sites.</p>
			
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
		</div>
	</div>
{% endwith %}
{% endblock %}
