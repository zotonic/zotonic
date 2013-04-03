<ul class="share-page-options">
	{% block twitter %}
	<li>
	    <a href="https://twitter.com/share?text={{ id.title|urlencode }}&amp;lang={{ z_language }}" onclick="return !window.open(this.href, 'Twitter', 'width=600,height=300,location=0,toolbar=0,scrollbars=0,status=0')" title="Twitter">{% image "lib/images/social/square/twitter.png" mediaclass="base-share-page" %}</a>
	</li>
	{% endblock %}
	{% block facebook %}
	<li>
    	<a href="http://www.facebook.com/sharer.php?u=http%3A%2F%2F{{ m.site.hostname }}{{ id.page_url|urlencode }}&amp;t={{ id.title|urlencode }}" onclick="return !window.open(this.href, 'Facebook', 'width=700,height=400,toolbar=0,location=0,scrollbars=0,status=0')" title="Facebook">{% image "lib/images/social/square/facebook.png" mediaclass="base-share-page" %}</a>
	</li>
	{% endblock %}
	{% all include "_share_page_option.tpl" id=id %}
</ul>
<div class="modal-footer">
    {% button class="btn btn-primary" text=_"Close" action={dialog_close} %}
</div>
