<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
<head>
    <meta charset="utf-8" />
    <title>An empty site</title>

    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<meta name="author" content="Your name here" />
	{% lib "bootstrap/css/bootstrap.css" %}
</head>

<body>
<div class="container">

    <div class="content" style="margin-top: 80px">
        <h1>Hi!</h1>

        <hr />
        <p>
            This is an empty Zotonic site, based on the <i>nodb</i>
            skeleton. That means that it doesn't use a database, and
            thus has no access to the authentication system or the
            admin. This nodb skeleton is mostly suited for
            framework-like applications which use their own, custom
            data storage.
        </p>
        
    </div>
</div>
</body>
</html>

