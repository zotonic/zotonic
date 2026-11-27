{% overrules %}

{% block bodyattr %} {% inherit %} data-fileuploader="{}" {% endblock %}

{% block content %}
    <p class="fileuploader-status" role="status" aria-live="polite"></p>
    {% inherit %}
    <div class="fileuploader-page-overlay">
        <span class="fileuploader-page-icon" aria-hidden="true">⇪</span>
        <h2>{_ Drop files to add them to Media _}</h2>
        <p>{_ Upload starts immediately. Each file becomes a new, published media item. _}</p>
        <p>{_ You can find and edit your uploads in Media. _}</p>
    </div>
{% endblock %}
