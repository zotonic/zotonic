<div class="embed-responsive" style="padding-top: {{ medium.oembed.height / medium.oembed.width * 100 }}%">
    {% if options.autoplay %}
        <iframe width="{{ medium.oembed.width }}" height="{{ medium.oembed.height }}"
                src="//player.vimeo.com/video/{{ medium.oembed.html|replace:".*player.vimeo.com/video/([0-9]*).*":"\\1" }}?autoplay=1"
                allow="autoplay" frameborder="0" allowfullscreen></iframe>
    {% else %}
        {{ medium.oembed.html }}
    {% endif %}
</div>