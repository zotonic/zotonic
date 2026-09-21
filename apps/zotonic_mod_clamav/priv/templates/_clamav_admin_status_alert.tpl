{% if m.acl.use.mod_admin %}
    {% if m.clamav.is_available == false %}
        <div class="alert alert-danger" role="alert">
            <strong>{_ Virus scanner unavailable _}</strong>
            {_ ClamAV could not be reached. Files cannot be uploaded until the virus scanner is available again. _}
            {_ Please contact your system administrator. _}
        </div>
    {% endif %}
{% endif %}
