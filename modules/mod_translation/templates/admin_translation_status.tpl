{% extends "admin_base.tpl" %}

{% block title %} {_ Translation status _} {% endblock %}

{% block content %}
<style type="text/css">
.perc span {
    padding-left: 5px;
}
.perc-0 { background-color: #ff0000; color: white; }
.perc-20 { background-color: #cc3300; color: white; }
.perc-40 { background-color: #996600; color: white; }
.perc-60 { background-color: #669900; color: white; }
.perc-80 { background-color: #33cc00; color: white; }
.perc-100 { background-color: #00ff00; color: white;}
</style>
<div id="content" class="zp-85">
	<div class="block clearfix">

		<h2>{_ Translation status per module _}</h2>

        <p>{_ To view the up-to-date version of this module, please run "generate .pot files" first from the translation overview. _}</p>
	
		<h3 class="above-list ">{_ Active modules _}</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-30">{_ Title _}</span>
                {% for code, lang in m.config.i18n.language_list.list %}
                {% if code != "en" %}
                <span class="zp-10">{{ lang.language }}</span>
                {% endif %}
                {% endfor %}
            </li>

		{% for sort, prio, module, props in modules %}
			<li id="{{ #li.module }}">
				<a href="#" class="clearfix">
					<span class="zp-30">{% include "_icon_status.tpl" status=status[module] status_id=#status.module %} {{ props.mod_title|default:props.title }}</span>
                    {% for code, lang in m.config.i18n.language_list.list %}
                    {% if code != "en" %}
                        {% admin_translation_statistics module=module lang=code %}
                    {% endif %}
                    {% endfor %}
                </a>
            </li>
            {% endfor %}
        </ul>
	</div>
</div>

{% endblock %}
