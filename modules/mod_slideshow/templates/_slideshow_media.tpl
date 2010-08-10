{# This template is used to show a slideshow with the {% media %} tag #}
{% if parts %}
<div class="slideshow-viewer {{ class }}" style="{{ style }}">
    {% ifequal parts|length 1 %}
		<div class="slideshow-viewer-image-wrapper">
	        {% media parts[1] width=width|default:slide_width height=height|default:slide_height %}
		</div>
    {% else %}
		<div class="slideshow-viewer-left-arrow"></div>
		<div class="slideshow-viewer-right-arrow"></div>

		<ul class="slideshow-viewer-image-wrapper do_cycle { pause: true, timeout: {{ slide_timeout|default:800 }}, speed: {{ slide_speed|default:2000 }}, fx: '{{ slide_fx|default:'scrollHorz' }}', easing: '{{ slide_easing|default:'easeInOutQuad' }}', prev: '.slideshow-viewer-left-arrow', next: '.slideshow-viewer-right-arrow'}" style="{% if slide_width|default:width %}width: {{ slide_width|default:width }}px;{% endif %}{% if slide_height|default:height %}height: {{ slide_height|default:height }}px; overflow:hidden;{% endif %}">
			{% for p_id in parts %}
				{% if m.rsc[p_id].is_visible %}
					{% catinclude "_slideshow_part.tpl" p_id %}
				{% endif %}
			{% endfor %}
		</ul>
    {% endifequal %}
</div>
{% endif %}
