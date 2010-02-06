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

			<div id="pubsub-subscriptions">{% include "_admin_pubsub_subscriptions_list.tpl" %}</div>
		</div>
	</div>
{% endwith %}
{% endblock %}
