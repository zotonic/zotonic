<div class="alert alert-danger" role="alert">
    {% if error == `history_expired` %}
        {_ Recipient history has expired. Go back and select all recipients to send again. _}
    {% elseif error == `overload` %}
        {_ The server is busy. Go back and try reviewing the mailing again in a moment. _}
    {% elseif error == `eacces` %}
        {_ You are not allowed to send this page. _}
    {% else %}
        {_ Could not count the eligible recipients. The list query may have taken too long. Go back and try again, or ask an administrator to check the mailing list. _}
    {% endif %}
    {_ Nothing has been sent. _}
</div>
{% include "_dialog_mailing_count_back.tpl" %}
