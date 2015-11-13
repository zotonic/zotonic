<a id="{{#email}}" href="#" title="{_ E-mail this page _}" class="btn btn-social-icon" style="background-color: #007c00; color: #fff;">
    <span class="icon-envelope" ></span>
</a>
{% wire id=#email action={dialog_close} action={dialog_mail_page id=id} %}
