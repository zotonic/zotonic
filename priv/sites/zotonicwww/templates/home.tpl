{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}home{% endblock %}

{% block banner %}

	<section id="banner">
		<section id="download-zotonic">
			<a href="/download" title="">
                <img src="/lib/images/download_zotonic.png" alt="" />
                <span class="header">Download Zotonic</span>
                <span class="subheader">Release {{ m.config.site.current_release.value }}</span>
            </a>
		</section>
		<section id="docs-zotonic">
			<a href="/documentation" title=""><img src="/lib/images/docs.png" alt="" />
                <span class="header">Zotonic documentation</span>
                <span class="subheader">{_ Read about getting up to speed fast _}</span>
            </a>
		</section>
	</section>

{% endblock %}

{% block content %}

	<article id="content" class="zp-67">
		<div class="padding">

			<h1>{{ m.rsc[id].title }}</h1>
			{{ m.rsc[id].body|show_media }}

		</div>
	</article>

{% endblock %}

{% block sidebar %}
	
	<aside id="sidebar" class="zp-33">
		<h2>Open Source</h2>
		<p>Zotonic is released under the Open Source <a href="/zotonic-license">Apache2 license</a>, which gives you the possibility to use it and modify it in every circumstance.</p>

        {% with m.rsc['page_gallery'].id as id %}
        <h2><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].short_title }} &raquo;</a></h2>
        <p>
            <a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].o.haspart|first width=320 %}</a>
            <a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].o.haspart|tail|first width=320 %}</a>
        </p>
        {% endwith %}

	</aside>

{% endblock %}
