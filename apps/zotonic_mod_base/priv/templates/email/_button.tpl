{% comment %}
    Centered button for emails.
    Arguments:
        - text : text for the button
        - url : link for the button
{% endcomment %}
<table align="center" role="presentation" cellspacing="0" cellpadding="0" border="0" style="margin: auto;">
    <tr>
        <td class="button-td button-td-primary" style="border-radius: 4px; background: #222222;">
             <a class="button-a button-a-primary" href="{{ url }}" style="background: #222222; border: 1px solid #000000; font-family: sans-serif; font-size: 15px; line-height: 15px; text-decoration: none; padding: 13px 17px; color: #ffffff; display: block; border-radius: 4px;">{{ text }}</a>
        </td>
    </tr>
</table>
