{% extends "page.tpl" %}

{% block title %}{_ Unsubscribe from mailing list _}{% endblock %}

{% block content %}

{% with m.mailinglist.confirm_key[q.confirm_key] as sub %}
<section id="content-wrapper" class="clearfix">
	<article id="content" class="zp-100">
		<div class="padding">
		{% if sub and m.rsc[sub.mailinglist_id].is_a.mailinglist %}
			<h1>{_ Unsubscribe from _} {{ m.rsc[sub.mailinglist_id].title|default:_"mailing list" }}</h1>
			<p>{_ Click <strong>unsubscribe</strong> to remove yourself from the mailing list. _}</p>
			<div id="confirm">
				{% button text=_"Unsubscribe"
						action={mailinglist_unsubscribe id=sub.id 
								on_success={update target="confirm" text=_"<p>Thank you. You are now unsubscribed.</p>"}
								on_error={update target="confirm" text=_"<p>Sorry, something went wrong. Please try again later.</p>"}} %}
			</div>

		{% else %}
			<h1>{_ Sorry, can’t find your subscription _}</h1>
			<p>{_ The key in your link does not match any subscription. Either you already unsubscribed, or the mailing list has been deleted. _}</p>
			<p>{_ Our excuses for the inconvenience. _}</p>
		{% endif %}
		</div>
	</article>
</section>
{% endwith %}

{% endblock %}
