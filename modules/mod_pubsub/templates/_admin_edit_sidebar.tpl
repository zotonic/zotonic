{% if not m.rsc[id].is_authoritative and m.rsc[id].pubsub_xmpp_uri %}
<div class="item-wrapper" id="sort-date">
    <h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
        <span class="title">Publish/subscribe</span>
        <span class="arrow">make smaller</span>
    </h3>
    <div class="item clearfix">
        <div class="admin-form form-item">
            <div class="notification notice">
                This item has been imported over XMPP from another website. <a href="{{ m.rsc[id].uri }}">Visit original item</a>
            </div>
            <fieldset>
                <div class="form-item">
                    <label>XMPP URI</label>
                    <input type="text" disabled="disabled" value="{{ m.rsc[id].pubsub_xmpp_uri }}" />
                </div>
            </fieldset>
        </div>
    </div>
</div>
{% endif %}
