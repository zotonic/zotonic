{% if blk.style == 'quote' %}
    <blockquote>
        {{ blk.body|show_media }}
    </blockquote>
{% elseif blk.style == 'aside' %}
    <aside class="block-text block-aside">{{ blk.body|show_media }}</aside>
{% else %}
    <div class="block-text">{{ blk.body|show_media }}</div>
{% endif %}
