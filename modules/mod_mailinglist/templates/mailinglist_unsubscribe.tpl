{% extends "page.tpl" %}

{% block title %}Unsubscribe from mailing list{% endblock %}

{% block content %}

{% with m.mailinglist.confirm_key[q.confirm_key] as sub %}
<section id="content-wrapper" class="clearfix">
	<article id="content" class="zp-80">
		<div class="padding">
		{% if sub and m.rsc[sub.mailinglist_id].is_a.mailinglist %}
			<h1>Unsubscribe from {{ m.rsc[sub.mailinglist_id].title|default:"mailing list" }}</h1>
			<p>Please click the button below to unsubscribe from the mailing list.</p>
			<div id="confirm">
				{% button text=["Unsubscribe ", sub.email|force_escape] 
						action={mailinglist_unsubscribe id=sub.id 
								on_success={update target="confirm" text="<p>Thank you. You are now unsubscribed.</p>"}
								on_error={update target="confirm" text="<p>Sorry, something went wrong. Please try again later.</p>"}} %}
			</div>

		{% else %}
			<h1>Sorry, can’t find your subscription</h1>
			<p>The key in your link does not match any subscription. Either you already unsubscribed, or the mailing list has been deleted.</p>
			<p>Our excuses for the inconvenience.</p>
		{% endif %}
		</div>
	</article>
	
	<aside id="sidebar" class="zp-20">
		<h2>Mailing lists</h2>
		
		<ul>
		{% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
			{% ifnotequal m.rsc[id].name "mailinglist_test" %}
				<li>
					<h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
					<p>{{ m.rsc[id].summary }}</p>
				</li>
			{% endifnotequal %}
		{% empty %}
			<li>No mailing lists</li>
		{% endfor %}
		</ul>
	</aside>
</section>
{% endwith %}

{% endblock %}