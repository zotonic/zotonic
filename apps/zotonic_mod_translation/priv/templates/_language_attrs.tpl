{% with language|default:id|language as lang %}xml:lang="{{ lang }}" lang="{{ lang }}" {% with lang|is_rtl|if:"rtl":"ltr" as dir %}dir="{{ dir }}" {% if class /= false %}class="{{ dir }} {{ class }}"{% endif %}{% endwith %}{% endwith %}