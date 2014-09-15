/*
Custom settings to override tiny-init.js.
*/
{% if m.config.mod_editor_tinymce.version.value < '4.0' %}
	tinyInit.language="en";
{% elseif z_language != `en` and z_language != `nl` %}
	tinyInit.language="en";
{% else %}
	tinyInit.language="{{ z_language|default:"en" }}";
{% endif %}
