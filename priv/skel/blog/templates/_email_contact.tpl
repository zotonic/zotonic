<html>
    <head>
        <title>{{ name|escape }} - Contact form</title>
    </head>
    <body>
        <p>Hello, the contact form of the site has been submitted.</p>

        <p>Name: {{ name|escape }}</p>
        <p>E-mail: {{ mail|escape }}</p>

        <p>The contents of the message was this:</p>
        <pre>{{ message|force_escape|linebreaksbr }}</pre>

        <p>Regards, your website.</p>
    </body>
</html>
