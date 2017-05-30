/*
Custom settings to override tiny-init.js.
*/
if (!tinyInit.language) {
    {% if m.config.mod_editor_tinymce.version.value < '4.0' %}
        tinyInit.language="en";
    {% elseif z_language != `en` and z_language != `nl` and z_language != `ru` %}
        tinyInit.language="en";
    {% else %}
        tinyInit.language="{{ z_language|default:"en" }}";
    {% endif %}
};
