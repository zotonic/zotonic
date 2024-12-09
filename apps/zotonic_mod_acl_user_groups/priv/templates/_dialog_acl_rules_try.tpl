<div class="modal-body">
	<p>{_ Use the following URL to test the access control rules before publishing them. _}</p>

	<p>{_ You can: _}</p>

	<ul>
		<li>{_ use a private or incognito window in your browser _}</li>
		<li>{_ use another browser (eg. Chrome if you are now using Firefox) _}</li>
		<li>{_ send this URL to somebody else for testing _}</li>
	</ul>

	<p>{_ The URL is valid for one day. _}</p>

	<input id="{{ #url }}" class="form-control" value="{% url acl_rule_test code=m.acl_rule.generate_code absolute_url %}" readonly>
</div>

{% javascript %}
	$('#{{ #url }}').focus().select().click(function() { $(this).select(); });
{% endjavascript %}

<div class="modal-footer clearfix">
    {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
</div>
