{% with m.rsc[id].o.document as docs %}
	{% if docs %}
		<h3>Downloadable attachments</h3>
		
		<ul class="list-filters">
		{% for doc in docs %}
			<li>
				<a href="{% url media_inline star=m.rsc[doc].medium.filename %}">{% image doc width=310 lossless alt=m.rsc[doc].title title=["Download",m.rsc[doc].title]|join:" " %}</a>
				<a href="{% url media_inline star=m.rsc[doc].medium.filename %}">{{ m.rsc[doc].title }}</a>
			</li>
		{% endfor %}
		</ul>
	{% endif %}
{% endwith %}