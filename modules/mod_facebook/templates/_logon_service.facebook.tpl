<h2>{_ One moment please _}</h2>
<p>{_ Redirecting to Facebook. _}</p>

{% javascript %}
	setTimeout(function() {
		window.location = '{% url facebook_authorize is_connect=is_connect %}';
	}, 100);
{% endjavascript %}
