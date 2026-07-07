{% extends "email_base.tpl" %}

{% block title %}{_ Your signup code _}: {{ code }}{% endblock %}

{% block body %}
<p>{_ Hello, _}</p>

<p>
    {% trans "Someone started signing up for an account at {site} using this email address." site=m.site.title %}
    {_ To continue, enter this code on the signup page: _}
</p>

<p style="font-size: 1.5em; font-weight: bold; text-align: center; margin: 20px 0 10px 0; border: 1px solid #c0c0f0; background-color: #f0f0fc; border-radius: 10px; padding: 10px; letter-spacing: 20%;">
    {{ code }}
</p>
<p style="text-align: center; color: #888; font-size: 0.8em;">{_ This code expires in 10 minutes. _}</p>

<p>
    <b>{_ Did you start this signup? _}</b>
    {_ Enter the code only on the signup page. Do not share it with anyone. _}
</p>

<p>
    <b>{_ Didn't start this signup? _}</b>
    {_ You can safely ignore this email. Someone may have entered your email address by mistake. _}
    {_ Without this code, they cannot continue signing up with your email address. _}
</p>

<p><em>{{ m.site.title }}</em></p>
{% endblock %}
