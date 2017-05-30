<h1>{_ Your site is ready _}</h1>

<p>{_ Congratulations, your site has been created and is now running. _}</p>

<p>{_ You can visit your site at: _} <a href="{{ site_url }}" target="_blank">{{ site_url }}</a></p>

{% if admin_url %}
    <p>{_ To manage your site: _}</p>
    <ol>
        <li>
            {_ Go to the admin interface at _} <a href="{{ admin_url }}" target="_blank">{{ admin_url }}</a>
        </li>
        <li>
            {_ Log on with username <b>admin</b> and password _} <b>{{ admin_password }}</b>
        </li>
    </ol>
{% endif %}

<p>
    {_ All the files and templates are placed at _} <tt>{{ site_dir }}</tt>
</p>
