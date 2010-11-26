{% extends "page.tpl" %}

{% block title %}Confirm subscription{% endblock %}

{% block content %}

<section id="content-wrapper" class="clearfix">
	<article id="content" class="zp-80">
		<div class="padding">
		{% with m.mailinglist.confirm_key[q.confirm_key] as confirm %}
			{% if confirm.mailinglist_id and confirm.email %}
				<h1>Subscribe to {{ m.rsc[confirm.mailinglist_id].title }}</h1>
	
				<p class="summary">{{ m.rsc[confirm.mailinglist_id].summary }}</p>
				
				<h2>{_ Please confirm your subscription _}</h2>
				
				<p>{_ Click the button below to confirm your subscription to this mailing list. _}</p>
				 
				<div id="confirm">
					{% button text=[_"Subscribe ", confirm.email|force_escape] 
							action={mailinglist_confirm confirm_key=q.confirm_key 
									on_success={update target="confirm" text=_"<p>Thank you. You are now subscribed.</p>"}
									on_error={update target="confirm" text=_"<p>Sorry, something went wrong. Please try to re-subscribe.</p>"}} %}
				</div>
				
			{% else %}
				<h1>{_ Sorry, can’t confirm your subscription _}</h1>
				
				<p>{_ The confirmation key is unknown. Either you already confirmed or something else went wrong. _}</p>
				<p>{_ You can try to re-subscribe to one of our mailing lists in the side column. _}</p>

			{% endif %}
		{% endwith %}
		</div>
	</article>
	
	<aside id="sidebar" class="zp-20">
		<h2>{_ Mailing lists _}</h2>
		
		<ul>
		{% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
			{% ifnotequal m.rsc[id].name "mailinglist_test" %}
				<li>
					<h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
					<p>{{ m.rsc[id].summary }}</p>
				</li>
			{% endifnotequal %}
		{% empty %}
			<li>{_ No mailing lists _}</li>
		{% endfor %}
		</ul>
	</aside>
</section>

{% endblock %}