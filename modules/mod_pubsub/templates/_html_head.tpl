<link rel="self" href="{{ m.rsc[id].uri }}" />
{% if id and m.rsc[id].pubsub_node %}
<link rel="xmpp.feed" href="xmpp:{{ m.config.mod_pubsub.pubsub_domain.value }}?;node={{ m.rsc[id].pubsub_node }}" title="XMPP updates for this item" />
{% endif %}
