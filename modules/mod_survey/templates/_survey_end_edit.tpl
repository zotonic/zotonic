<h2>{_ Saved _}</h2>

<p>{_ The results have been saved. _}</p>

<p>
    <a href="#" id="{{ #close }}" style="display:none" class="btn btn-primary">{_ Close _}</a>
    <a href="#" id="{{ #back_history }}" style="display:none" class="btn btn-primary">{_ Back _}</a>
    <a href="{{ id.page_url }}" id="{{ #back_survey }}" style="display:none" class="btn btn-primary">{_ Back _}</a>
</p>

{% javascript %}
    if (window.opener) {
        $('#{{ #close }}')
            .show()
            .click(function() {
                try {
                    window.opener.z_reload();
                } catch (_E) { };
                window.close();
                return false;
            });
    } else if (window.history.length > 0) {
        $('#{{ #back_history }}')
            .show()
            .click(function() {
                window.history.go(-1);
                return false;
            });
    } else {
        $('#{{ #back_survey }}')
            .show()
    }
{% endjavascript %}
