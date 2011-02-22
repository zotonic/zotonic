{# This template is used to show a slideshow with the {% media %} tag #}
{% if parts %}
<div class="slideshow-viewer {{ class }}" style="{{ style }}">
    {% ifequal parts|length 1 %}
		<div class="slideshow-viewer-image-wrapper">
			{% catinclude "_slideshow_part.tpl" parts[1]
			 	width=width height=height
				slide_width=slide_width slide_height=slide_height
				start_id=start_id
			%}
		</div>
    {% else %}
		<div id="{{ #slideshow_left_arrow }}" class="slideshow-viewer-left-arrow"></div>
		<div id="{{ #slideshow_right_arrow }}" class="slideshow-viewer-right-arrow"></div>

		<ul class="slideshow-viewer-image-wrapper do_cycle" data-cycle="pause: true, timeout: {{ slide_timeout|default:(m.config.mod_slideshow.timeout.value)|default:2000 }}, speed: {{ slide_speed|default:(m.config.mod_slideshow.speed.value)|default:2000 }}, fx: '{{ slide_fx|default:'scrollHorz' }}', easing: '{{ slide_easing|default:'easeInOutQuad' }}', prev: '#{{ #slideshow_left_arrow }}', next: '#{{ #slideshow_right_arrow }}'" style="{% if slide_width|default:width %}width: {{ slide_width|default:width }}px;{% endif %}{% if slide_height|default:height %}height: {{ slide_height|default:height }}px; overflow:hidden;{% endif %}">
			{% for p_id in parts %}
				{% if m.rsc[p_id].is_visible %}
					<li style="{% if slide_width %}width: {{ slide_width }}px;{% endif %}{% if slide_height %}height: {{ slide_height }}px; overflow:hidden;{% endif %}">
						{% catinclude "_slideshow_part.tpl" p_id
						 	width=width height=height
							slide_width=slide_width slide_height=slide_height
							start_id=start_id
						%}
					</li>
				{% endif %}
			{% endfor %}
		</ul>
    {% endifequal %}
</div>
{% endif %}
