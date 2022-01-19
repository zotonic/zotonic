/*
Custom settings to override tiny-init.js.
*/
if (!tinyInit.language) {
    {% if z_language != `en` and z_language != `nl` and z_language != `ru` %}
        tinyInit.language="en";
    {% else %}
        tinyInit.language="{{ z_language|default:"en" }}";
    {% endif %}
};
