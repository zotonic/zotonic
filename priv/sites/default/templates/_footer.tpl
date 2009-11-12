<div id="footer-content" class="clearfix"> 
	<div class="zp-33">
		<h5>Latest blog posts</h5>
		<ul class="footer-list">
		{% for id in m.search[{latest cat="blog" pagelen="4"}] %}
				<li><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].publish_date }}&mdash;{{ m.rsc[id].title }}</a></li>
			{% endfor %}
		</ul>
	</div>

	<div class="zp-33">
		<h5>Latest changes</h5>
		<ul class="footer-list">
		{% for id in m.search[{latest cat="text" pagelen="4"}] %}
				<li><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></li>
			{% endfor %}
		</ul>
	</div>

	<div class="zp-33">
		<p>zotonic.com &copy; 2009</p>
		{% include "_edit_button.tpl" %}
	</div>
</div>
