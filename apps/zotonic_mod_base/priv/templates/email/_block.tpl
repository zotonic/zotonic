{% comment %}
    Block for in the below_body part.
{% endcomment %}
<tr>
    <td style="background-color: #ffffff;">
        <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
            <tr>
                <td style="padding: 20px 20px 10px 20px; font-family: sans-serif; font-size: 15px; line-height: 20px; color: #555555;">
                    {% block body %}
                        {{ body|show_media:"email/_body_media.tpl" }}
                    {% endblock %}
                </td>
            </tr>
        </table>
    </td>
</tr>
