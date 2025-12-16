{% extends "email_base.tpl" %}

{% block title %}{_ Maximum submissions reached _}: {{ id.title }}{% endblock %}

{% block body %}

<p>{_ Hello, _}</p>

<p>{% trans 'The maximum number of submissions for <a href="{url}"><b>{title}</b></a> on {site} has been reached.'
        title=id.title|default:id.short_title|default:_"Untitled"
        url=id.page_url_abs
        site=m.site.title
%}</p>

<p>{% trans 'There are now <b>{n}</b> submissions out of a maximum of {max}.'
        n=results_count
        max=max_results
%}</p>

<p>{_ You received this email because your email address is configured in the settings as the handling address. _}</p>

{% endblock %}
