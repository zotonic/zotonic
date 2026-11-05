<h4>{{ check_title|escape }}</h4>
{# Keep the heading and a result paragraph in place while checking. The fixed
   image dimensions prevent the loader from changing the paragraph height. #}
<div id="{{ #check }}" role="status" aria-live="polite">
    <p><img src="{% lib_url "images/spinner.gif" %}" width="16" height="16" alt=""> {_ Checking… _}</p>
</div>
{% wire type="load" action={update target=#check template=check_template show_status} %}
