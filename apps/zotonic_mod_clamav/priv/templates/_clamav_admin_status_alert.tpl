{% if m.acl.use.mod_admin %}
    {% if m.clamav.is_available == false %}
        <div class="alert alert-danger" role="alert">
            <strong>{_ Virus scanner unavailable _}</strong>
            {_ ClamAV could not be reached. Files cannot be uploaded until the virus scanner is available again. _}
            {_ Please contact your system administrator. _}
        </div>
    {% elseif show_status %}
        <p class="text-success"><span class="glyphicon glyphicon-ok-sign" aria-hidden="true"></span> {_ Virus scanner available. _}</p>
    {% endif %}
{% endif %}
