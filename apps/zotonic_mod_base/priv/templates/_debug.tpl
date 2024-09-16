<style type="text/css" nonce="{{ m.req.csp_nonce }}">
    .z-debug-holder {
        overflow: hidden;
        border-radius: 0.5rem;
        resize: horizontal;
        min-width: -webkit-fill-available;
    }

    .z-debug-vars-holder {
        border: 1px solid #14A7DF;
        border-top: none;
        border-bottom-left-radius: 0.5rem;
        border-bottom-right-radius: 0.5rem;
        background-color: white;
        overflow: auto;
    }

    .z-debug-summary {
        position: relative;
    }

    .z-debug:not([open]) .z-debug-summary .z-debug-btn-collapse,
    .z-debug:not([open]) .z-debug-summary .z-debug-btn-expand {
        display: none;
    }

    .z-debug-btns-holder {
        position: absolute;
        top: 0;
        right: 0;
        display: flex;
        align-items: stretch;
        height: 5rem;
    }

    .z-debug-btns-holder button {
        all: unset;
        cursor: pointer;
        padding: 1rem;
        color: white;
        font-size: 2rem;
        text-align: center;
        transition: background-color 100ms ease-in-out, color 100ms ease-in-out;
        display: flex;
        align-items: center;
    }

    .z-debug-btns-holder button:not(.z-debug-btn-times):hover {
        background-color: #44609d;
    }

    .z-debug-btn-times:hover {
        background-color: crimson;
    }

    .z-debug-btn-times:hover {
        background-color: crimson;
    }

    .z-debug summary {
        display: list-item;
        font-weight: bold;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .z-debug summary:hover {
        cursor: pointer;
    }

    .z-debug-summary {
        padding: 1.5rem 1rem;
        background-color: #14A7DF;
        color: white;
    }

    .z-debug :not(details[open]) .z-debug-var-summary {
        padding: 1rem;
    }

    .z-debug details:not(:last-child) .z-debug-var-summary {
        border-bottom: 1px solid #ccc;
    }

    .z-debug pre {
        margin: 0;
        border: none;
    }
</style>

<div id="{{ #z_debug_holder }}" class="z-debug-holder">
    <details class="z-debug" open>
        <summary class="z-debug-summary">
            <span>DEBUG</span>
            <div class="z-debug-btns-holder">
                <button type="button" class="z-debug-btn-collapse" id="{{ #debug_details }}">
                    &lowbar;
                </button>
                <button type="button" class="z-debug-btn-expand">
                    &square;
                </button>
                <button type="button" class="z-debug-btn-times">
                    &times;
                </button>
            </div>
        </summary>
        <div class="z-debug-vars-holder">
            {% for name, value in vars %}
                <details>
                    <summary class="z-debug-var-summary">{{ name }}</summary>
                    <pre class="language-erlang"><code>{{ value }}</code></pre>
                </details>
            {% endfor %}
        </div>
    </details>

    {% if m.req.is_http_request %}
        {# Inline, so that pages without a script-scomp work as well #}
        <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
          document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-collapse")
              .addEventListener(
                  "click",
                  (e) => document.querySelectorAll("#{{ #z_debug_holder }} .z-debug details")
                          .forEach(d => d.removeAttribute("open")) );

          document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-expand")
              .addEventListener(
                  "click",
                  (e) => document.querySelectorAll("#{{ #z_debug_holder }} .z-debug details")
                            .forEach(d => d.setAttribute("open", true)) );

          document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-times")
              .addEventListener(
                  "click",
                  (e) => document.getElementById("{{ #z_debug_holder }}").remove() );
        </script>
    {% else %}
        {% javascript %}
            document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-collapse")
                .addEventListener(
                    "click",
                    (e) => document.querySelectorAll("#{{ #z_debug_holder }} .z-debug details")
                            .forEach(d => d.removeAttribute("open")) );

            document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-expand")
                .addEventListener(
                    "click",
                    (e) => document.querySelectorAll("#{{ #z_debug_holder }} .z-debug details")
                            .forEach(d => d.setAttribute("open", true)) );

            document.querySelector("#{{ #z_debug_holder }} .z-debug-btn-times")
                .addEventListener(
                    "click",
                    (e) => document.getElementById("{{ #z_debug_holder }}").remove() );
        {% endjavascript %}
    {% endif %}
</div>
