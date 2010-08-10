{# This template is used to show a slideshow with the {% media %} tag #}
{% if parts %}
<div class="slideshow-viewer">
    {% ifequal parts|length 1 %}
		<div class="slideshow-viewer-image-wrapper">
	        {% media parts[1] %}
		</div>
    {% else %}
		<div class="slideshow-viewer-left-arrow"></div>
		<div class="slideshow-viewer-right-arrow"></div>

		<ul class="slideshow-viewer-image-wrapper {{ class }} do_cycle { pause: true, timeout: 8000, speed: 2000, fx: 'scrollHorz', easing: 'easeInOutQuad', prev: '.slideshow-viewer-left-arrow', next: '.slideshow-viewer-right-arrow'}" style="{% if width %}width: {{ width }}px;{% endif %}{% if height %}height: {{ height }}px;{% endif %}{{ style }}">
			{% for p_id in parts %}
				{% if m.rsc[p_id].is_visible %}
					{% catinclude "_slideshow_part.tpl" p_id %}
				{% endif %}
			{% endfor %}
		</ul>
    {% endifequal %}
</div>
{% endif %}
