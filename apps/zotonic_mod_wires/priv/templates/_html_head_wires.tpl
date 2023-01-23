{# Initialize Zotonic - set promises
 #}
<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    var zotonic = zotonic || {};

    zotonic.wiresLoaded = new Promise( (resolve) => { zotonic.wiresLoadedResolve = resolve; } );
</script>
