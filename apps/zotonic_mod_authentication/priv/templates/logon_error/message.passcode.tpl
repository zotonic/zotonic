<p class="text-danger">
    {_ The two-factor authentication passcode is not correct. Please try again. _}
</p>

{% if not hide_links %}
    <p>
        <a href="{% url logon_reminder %}" id="logon_error_link_reminder"
           data-onclick-topic="model/auth-ui/post/view/reminder">{_ Forgot your password? _}</a>
    </p>
{% endif %}
