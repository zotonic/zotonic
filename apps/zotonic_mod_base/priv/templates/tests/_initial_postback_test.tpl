
<p>Loaded #{{ n|escape }}</p>

{% if n < till %}
    <script>
        document.location = "/test/initial_postback_test?n={{ n }}";
    </script>
{% else %}
    <p>Success!</p>
{% endif %}
