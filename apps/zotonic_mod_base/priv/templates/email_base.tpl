<!DOCTYPE html>
<html lang="{{ z_language }}" xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office">
{% block email %}
<head>
    {% comment %} Email template from https://tedgoas.github.io/Cerberus/ {% endcomment %}
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    {% comment %} Disable auto-scale in iOS 10 Mail entirely {% endcomment %}
    <meta name="x-apple-disable-message-reformatting">
    {% comment %} Tell iOS not to automatically link certain text strings. {% endcomment %}
    <meta name="format-detection" content="telephone=no,address=no,email=no,date=no,url=no">
    <meta name="color-scheme" content="light">
    <meta name="supported-color-schemes" content="light">

    {% comment %} The title tag is used for the email subject {% endcomment %}
    <title>{% block title_wrapper %}{% block title %}{{ id.title }}{% endblock %}{% endblock %}</title>

    {% comment %} What it does: Makes background images in 72ppi Outlook render at correct size. {% endcomment %}
    <!--[if gte mso 9]>
    <xml>
        <o:OfficeDocumentSettings>
            <o:AllowPNG/>
            <o:PixelsPerInch>96</o:PixelsPerInch>
        </o:OfficeDocumentSettings>
    </xml>
    <![endif]-->

    <!-- CSS Reset : BEGIN -->
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">

        /* What it does: Tells the email client that only light styles are provided but the client can transform them to dark. A duplicate of meta color-scheme meta tag above. */
        :root {
          color-scheme: light;
          supported-color-schemes: light;
        }

        /* What it does: Remove spaces around the email design added by some email clients. */
        /* Beware: It can remove the padding / margin and add a background color to the compose a reply window. */
        html,
        body {
            margin: 0 auto !important;
            padding: 0 !important;
            height: 100% !important;
            width: 100% !important;
        }

        /* What it does: Stops email clients resizing small text. */
        * {
            -ms-text-size-adjust: 100%;
            -webkit-text-size-adjust: 100%;
        }

        /* What it does: Centers email on Android 4.4 */
        div[style*="margin: 16px 0"] {
            margin: 0 !important;
        }

        /* What it does: forces Samsung Android mail clients to use the entire viewport */
        #MessageViewBody, #MessageWebViewDiv{
            width: 100% !important;
        }

        /* What it does: Stops Outlook from adding extra spacing to tables. */
        table,
        td {
            mso-table-lspace: 0pt !important;
            mso-table-rspace: 0pt !important;
        }

        /* What it does: Fixes webkit padding issue. */
        table {
            border-spacing: 0 !important;
            border-collapse: collapse !important;
            table-layout: fixed !important;
            margin: 0 auto !important;
        }

        /* What it does: Uses a better rendering method when resizing images in IE. */
        img {
            -ms-interpolation-mode:bicubic;
        }

        /* What it does: Prevents Windows 10 Mail from underlining links despite inline CSS. Styles for underlined links should be inline. */
        a {
            text-decoration: none;
        }

        /* What it does: A work-around for email clients meddling in triggered links. */
        a[x-apple-data-detectors],  /* iOS */
        .unstyle-auto-detected-links a,
        .aBn {
            border-bottom: 0 !important;
            cursor: default !important;
            color: inherit !important;
            text-decoration: none !important;
            font-size: inherit !important;
            font-family: inherit !important;
            font-weight: inherit !important;
            line-height: inherit !important;
        }

        /* What it does: Prevents Gmail from displaying a download button on large, non-linked images. */
        .a6S {
            display: none !important;
            opacity: 0.01 !important;
        }

        /* What it does: Prevents Gmail from changing the text color in conversation threads. */
        .im {
            color: inherit !important;
        }

        /* If the above doesn't work, add a .g-img class to any image in question. */
        img.g-img + div {
            display: none !important;
        }

        {% comment %}
            What it does: Removes right gutter in Gmail iOS app: https://github.com/TedGoas/Cerberus/issues/89
            Create one of these media queries for each additional viewport size you'd like to fix
        {% endcomment %}

        /* iPhone 4, 4S, 5, 5S, 5C, and 5SE */
        @media only screen and (min-device-width: 320px) and (max-device-width: 374px) {
            u ~ div .email-container {
                min-width: 320px !important;
            }
        }
        /* iPhone 6, 6S, 7, 8, and X */
        @media only screen and (min-device-width: 375px) and (max-device-width: 413px) {
            u ~ div .email-container {
                min-width: 375px !important;
            }
        }
        /* iPhone 6+, 7+, and 8+ */
        @media only screen and (min-device-width: 414px) {
            u ~ div .email-container {
                min-width: 414px !important;
            }
        }

    </style>
    <!-- CSS Reset : END -->

    <!-- Progressive Enhancements : BEGIN -->
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">

        /* What it does: Hover styles for buttons */
        .button-td,
        .button-a {
            transition: all 100ms ease-in;
        }
        .button-td-primary:hover,
        .button-a-primary:hover {
            background: #555555 !important;
            border-color: #555555 !important;
        }

        /* Media Queries */
        @media screen and (max-width: 600px) {

            /* What it does: Adjust typography on small screens to improve readability */
            .email-container p {
                font-size: 17px !important;
            }

        }

    </style>
    <!-- Progressive Enhancements : END -->

</head>
{% comment %}
    The email background color (#ffffff) is defined in three places:
    1. body tag: for most email clients
    2. center tag: for Gmail and Inbox mobile apps and web versions of Gmail, GSuite, Inbox, Yahoo, AOL, Libero, Comcast, freenet, Mail.ru, Orange.fr
    3. mso conditional: For Windows 10 Mail
{% endcomment %}
{% with bgcolor|default:"#ffffff" as bgcolor %}
<body width="100%" style="margin: 0; padding: 0 !important; mso-line-height-rule: exactly; background-color: {{ bgcolor }};">
    <center role="article" aria-roledescription="email" lang="en" style="width: 100%; background-color: {{ bgcolor }};">
    <!--[if mso | IE]>
    <table role="presentation" border="0" cellpadding="0" cellspacing="0" width="100%" style="background-color: {{ bgcolor }};">
    <tr>
    <td>
    <![endif]-->

        <!-- Visually Hidden Preheader Text : BEGIN -->
        <div style="max-height:0; overflow:hidden; mso-hide:all;" aria-hidden="true">{% filter trim %}
            {% comment %}
            (Optional) This text will appear in the inbox preview, but not the email body. It can be used to supplement the email subject line or even summarize the email's contents. Extended text preheaders (~490 characters) seems like a better UX for anyone using a screenreader or voice-command apps like Siri to dictate the contents of an email. If this text is not included, email clients will automatically populate it using the text (including image alt text) at the start of the email's body.
            {% endcomment %}
            {% block preheader %}
                {% with id.summary.length as len %}
                    {% if not len or len > 300 %}{{ id|summary:490 }}
                    {% else %}{{ id|summary }} {{ id.body|striptags|truncate:(490-len) }}
                    {% endif %}
                {% endwith %}
            {% endblock %}
        {% endfilter %}</div>
        <!-- Visually Hidden Preheader Text : END -->

        {% comment %}
            Create white space after the desired preview text so email clients don’t pull other distracting text into the inbox preview. Extend as necessary.
        {% endcomment %}
        <!-- Preview Text Spacing Hack : BEGIN -->
        <div style="display: none; font-size: 1px; line-height: 1px; max-height: 0px; max-width: 0px; opacity: 0; overflow: hidden; mso-hide: all; font-family: sans-serif;" aria-hidden="true">
            &zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;
        </div>
        <!-- Preview Text Spacing Hack : END -->

        {% comment %}
            The content comment below is for the Zotonic email server
            It signifies where the text content of the email starts
            The HTML text after this will be used for the text part of an email
        {% endcomment %}
        <!--content-->


        {% filter replace:'<h1>':'<h1 style="margin: 0 0 10px 0; font-family: sans-serif; font-size: 25px; line-height: 30px; color: #333333; font-weight: normal;">' %}
        {% filter replace:'<h2>':'<h2 style="margin: 0 0 10px 0; font-family: sans-serif; font-size: 20px; font-weight: normal;">' %}
        {% filter replace:'<h3>':'<h3 style="margin: 0 0 10px 0; font-family: sans-serif; font-size: 18px;">' %}
        {% filter replace:'<h4>':'<h4 style="margin: 0 0 10px 0; font-family: sans-serif; font-size: 16px; font-weight: bold;">' %}
        {% filter replace:'<p>':'<p style="margin: 0 0 10px 0;">' %}
        {% filter replace:'<ul>':'<ul style="padding: 0; margin: 10px 0 10px 0; list-style-type: disc;">' %}
        {% filter replace:'<li>':'<li style="margin:0 0 10px 30px;">' %}

        {% comment %}
            Set the email width. Defined in two places:
            1. max-width for all clients except Desktop Windows Outlook, allowing the email to squish on narrow but never go wider than 600px.
            2. MSO tags for Desktop Windows Outlook enforce a 600px width.
        {% endcomment %}
        <div style="max-width: 600px; margin: 0 auto;" class="email-container">
            <!--[if mso]>
            <table align="center" role="presentation" cellspacing="0" cellpadding="0" border="0" width="600">
            <tr>
            <td>
            <![endif]-->

            <!-- Email Body : BEGIN -->
            <table align="center" role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%" style="margin: 20px auto;">

                {% block above_body %}
                {% endblock %}

                {% comment %}
                <!-- Email Header : BEGIN -->
                <tr>
                    <td style="padding: 20px 0; text-align: center">
                        <img src="https://via.placeholder.com/200x50" width="200" height="50" alt="alt_text" border="0" style="height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555;">
                        }
                    </td>
                </tr>
                <!-- Email Header : END -->
                {% endcomment %}

                {% comment %} Hero Image, Flush : BEGIN {% endcomment %}
                {% block depiction %}
                    {% if id.depiction or depiction %}
                    <tr>
                        <td style="background-color: #ffffff;">
                            {% if id %}<a href="{{ id.page_url_abs }}">{% endif %}
                            <img src="{% image_url depiction|default:id.depiction upscale width=1200 height=600 crop absolute_url %}" width="600" height="" alt="{{ id.depiction.title }}" border="0" style="width: 100%; max-width: 600px; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: auto; display: block;" class="g-img">
                            {% if id %}</a>{% endif %}
                        </td>
                    </tr>
                    {% endif %}
                {% endblock %}
                {% comment %} Hero Image, Flush : END {% endcomment %}

                {% comment %} 1 Column Text : BEGIN {% endcomment %}
                <tr>
                    <td style="background-color: #ffffff;">
                        <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
                            {% block content %}
                            <tr>
                                <td style="padding: 20px 20px 10px 20px; font-family: sans-serif; font-size: 15px; line-height: 20px; color: #555555;">
                                    <div {% optional include "_language_attrs.tpl" %}>
                                        <h1>{% block body_title %}{{ id.title }}{% endblock %}</h1>
                                        {% block body %}
                                            {% if id.summary %}
                                                <p><b>{{ id.summary|linebreaksbr }}</b></p>
                                            {% endif %}
                                            {{ id.body|show_media:"email/_body_media.tpl" }}
                                        {% endblock %}
                                    </div>
                                </td>
                            </tr>
                            {% endblock %}
                        </table>
                    </td>
                </tr>
                {% comment %} 1 Column Text : END {% endcomment %}

                {% block below_body %}
                    {% if id.is_a.text %}
                        {% for blk in m.rsc[id].blocks %}
                            {% optional include ["email/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
                        {% endfor %}
                    {% endif %}
                {% endblock %}

{% comment %}

                <!-- Clear Spacer : BEGIN -->
                <tr>
                    <td aria-hidden="true" height="40" style="font-size: 0px; line-height: 0px;">
                        &nbsp;
                    </td>
                </tr>
                <!-- Clear Spacer : END -->

                <!-- 1 Column Text : BEGIN -->
                <tr>
                    <td style="background-color: #ffffff;">
                        <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
                            <tr>
                                <td style="padding: 20px; font-family: sans-serif; font-size: 15px; line-height: 20px; color: #555555;">
                                    <p style="margin: 0;">Maecenas sed ante pellentesque, posuere leo id, eleifend dolor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Praesent laoreet malesuada cursus. Maecenas scelerisque congue eros eu posuere. Praesent in felis ut velit pretium lobortis rhoncus ut&nbsp;erat.</p>
                                </td>
                            </tr>
                        </table>
                    </td>
                </tr>
                <!-- 1 Column Text : END -->
{% endcomment %}

            </table>
            <!-- Email Body : END -->

            <!-- Email Footer : BEGIN -->
            <table align="center" role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%" style="margin: auto;">
                <tr>
                    <td style="padding: 20px 20px 10px 20px; font-family: sans-serif; font-size: 14px; line-height: 15px; text-align: center; color: #ffffff;">
                        {% block footer %}
                            {% if id %}
                                <p><a href="{{ id.page_url_abs }}" style="color: #ffffff; text-decoration: underline; font-weight: bold;">{_ Read this page on the web _}</a></p>
                            {% endif %}
                        {% endblock %}
                    </td>
                </tr>
            </table>
            <!-- Email Footer : END -->

            <!--[if mso]>
            </td>
            </tr>
            </table>
            <![endif]-->
        </div>

        <!-- Full Bleed Background Section : BEGIN -->
        {% block footer_full_width %}
            {% if is_footer_full_width %}
                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%" style="background-color: #709f2b;">
                    <tr>
                        <td>
                            <div align="center" style="max-width: 600px; margin: auto;" class="email-container">
                                <!--[if mso]>
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="600" align="center">
                                <tr>
                                <td>
                                <![endif]-->
                                <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
                                    <tr>
                                        <td style="padding: 20px; text-align: left; font-family: sans-serif; font-size: 15px; line-height: 20px; color: #ffffff;">
                                            {% block footer_full_width_content %}{% endblock %}
                                        </td>
                                    </tr>
                                </table>
                                <!--[if mso]>
                                </td>
                                </tr>
                                </table>
                                <![endif]-->
                            </div>
                        </td>
                    </tr>
                </table>
            {% endif %}
        {% endblock %}
        <!-- Full Bleed Background Section : END -->

        {% endfilter %}
        {% endfilter %}
        {% endfilter %}
        {% endfilter %}
        {% endfilter %}
        {% endfilter %}
        {% endfilter %}

    <!--[if mso | IE]>
    </td>
    </tr>
    </table>
    <![endif]-->
    </center>
</body>
{% endwith %}
{% endblock %}
</html>
