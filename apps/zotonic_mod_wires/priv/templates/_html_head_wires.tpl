{# Initialize Zotonic - set promises and queue early form submits
 #}
{% block style %}
<style type="text/css" nonce="{{ m.req.csp_nonce }}">
    .z-wires-submitting {
        pointer-events: none;
        opacity: 0.5;
        background: url('/lib/images/spinner.gif') no-repeat center center;
    }
    body:has(.z-wires-submitting) {
        cursor: wait;
    }
</style>
{% endblock %}
<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    var zotonic = zotonic || {};

    zotonic.wiresLoaded = new Promise( (resolve) => { zotonic.wiresLoadedResolve = resolve; } );
    zotonic.wiresReady = new Promise( (resolve) => { zotonic.wiresReadyResolve = resolve; } );

    function zInitCatchSubmit(event) {
        if (event.target.tagName === 'FORM' && event.target.getAttribute('action') === 'postback') {
            event.preventDefault();
            event.target.classList.add('z-wires-submitting');
            zotonic.wiresReady.then(function() {
                setTimeout(() => {
                    event.target.dispatchEvent(event);
                    event.target.classList.remove('z-wires-submitting');
                }, 10);
            });
        }
    };
    document.documentElement.addEventListener('submit', zInitCatchSubmit);
    zotonic.wiresReady.then(function() {
        document.documentElement.removeEventListener('submit', zInitCatchSubmit);
    });
</script>
