// Cookie consent: manage cookies and hide/show elements depending on the
// cookie consent preferences of users. Also add simple consent form to
// accept or deny cookies.
//
// The cookie consent permissions request should only be shown if there are
// elements on the page that set cookies (or track visitors) and the cookie
// consent is not yet requested.

(function() {

    let is_consent_requested = false;

    // If never consented and we have elements that need a consent then
    // the cookie consent dialog is shown. The event is registerd in _body_html.tpl
    // Ask at most once per page.
    function maybe_trigger_cookie_consent() {
        if (!is_consent_requested && !z_cookie_consent_given()) {
            is_consent_requested = true;
            trigger_cookie_consent();
        }
    }

    function trigger_cookie_consent() {
        setTimeout(
            () => {
                if (z_registered_events["cookie-consent"]) {
                    z_event("cookie-consent", {});
                } else {
                    // Race condition, event not yet registered,
                    // try again in 100msec
                    trigger_cookie_consent();
                }
            },
            100);
    }

    function enableElement($elt) {
        switch ($elt.prop("tagName")) {
            case 'LINK':
                // CSS
                $elt.attr('type', 'text/css');
                $elt.replaceWith($elt[0].outerHTML);
                break;
            case 'SCRIPT':
                // Javascript
                replacement = $elt[0].cloneNode(true);
                replacement.nonce = z_script_nonce;
                replacement.setAttribute('type', 'text/javascript');
                replacement.removeAttribute('data-cookie-consent');
                $elt.replaceWith(replacement);
                break;
            case 'IFRAME':
                // Use src-cookie-consent
                if ($elt.attr('data-cookie-consent-src')) {
                    $elt.attr('src', $elt.attr('data-cookie-consent-src'));
                    $elt.removeAttr('data-cookie-consent-src');
                }
                break;
            default:
                // Replace with stored content
                const $consented = $elt.find('[type="text/x-cookie-consented"]');
                if ($consented.length > 0) {
                    let $replace = $(html_unescape($consented.text()));
                    $elt.replaceWith($replace);
                    if (typeof($.widgetManager) != 'undefined') {
                        $replace.widgetManager();
                    }
                } else {
                    window.location.reload(true);
                }
                break;
        }
    }

    // Sync consent status as events to Matomo or GTM
    function setTrackingEvents( consent ) {
        let events;

        switch (consent) {
            case 'all':
                events = [
                    "cookie_consent_functional",
                    "cookie_consent_statistics",
                    "cookie_consent_marketing"
                ];
                break;
            case 'stats':
                events = [
                    "cookie_consent_functional",
                    "cookie_consent_statistics"
                ];
                break;
            case 'functional':
            default:
                events = [
                    "cookie_consent_functional"
                ];
                break;
        }

        events.forEach(function (event) {
            // GTM
            window.dataLayer = window.dataLayer || [];
            window.dataLayer.push({ event: event });

            // Matomo
            window._mtm = window._mtm || [];
            window._mtm.push({ event: event });
        });
    }

    // Check the current consent status and disable elements with incompatible
    // data-cookie-consent attributes or enable script tags with the correct attribute.
    // Script and css tags can also have the 'type'-attribute 'text/x-cookie-consent-*'
    // to delay execution till the visitor consented to those cookies.
    function syncConsent() {
        const consent = z_cookie_consent_fetch();

        const $elts = $(".do_cookie_consent,iframe[data-cookie-consent-src],link[type='text/x-cookie-consent'],script[type='text/x-cookie-consent']");
        $elts.each(function() {
            const consent_needed = $(this).attr('data-cookie-consent') || "all";

            if (z_cookie_consented(consent_needed)) {
                enableElement($(this));
            } else {
                maybe_trigger_cookie_consent();
            }
        });

        setTrackingEvents(consent);
    }

    function init() {
        // do_cookie_consent widget
        $.widget("ui.cookie_consent",
        {
            _init: function() {
                const $elt = $(this.element);
                let consent_needed = 'all';

                switch ($elt.attr('data-cookie-consent')) {
                    case 'functional':
                        consent_needed = 'functional';
                        break;
                    case 'stats':
                        consent_needed = 'stats';
                        break;
                    case 'all':
                    default:
                        consent_needed = 'all';
                        break;
                }

                if (z_cookie_consented(consent_needed)) {
                    // Replace the element with its wrapped HTML content.
                    enableElement($elt);
                } else {
                    maybe_trigger_cookie_consent();
                }
            }
        });

        $.ui.cookie_consent.defaults = {
        };

        // Links to change cookie settings on media item placeholders
        $('body').on('click', '.cookie-consent-change,.cookie-consent-preview,a[href="#cookie-consent"]', function(ev) {
            ev.stopImmediatePropagation();
            ev.preventDefault();
            if ($('#cookie-consent').length == 0) {
                z_event("cookie-consent", {});
            }
        });

        // Initialize all elements on the page.
        $(function() {
            syncConsent()

            // Trigger on further consent changes.
            window.addEventListener("zotonic:cookie-consent", function(ev) {
                syncConsent();
            });
        });
    }

    if (typeof zotonic == "object" && typeof zotonic.wiresLoaded == "object") {
        zotonic.wiresLoaded.then( () => init() );
    } else {
        init();
    }
})();
