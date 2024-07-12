/* Error handling
----------------------------------------------------------

Fetch the error event and log it to the server.
Which should log it in a separate ui error log.

---------------------------------------------------------- */

(function() {
    var oldOnError = window.onerror;
    var isUnloading = false;

    window.addEventListener("pagehide", function(event) {
        isUnloading = true;
    });

    window.addEventListener("beforeunload", function(event) {
        isUnloading = true;
        setTimeout(function() { isUnloading = false }, 5000);
    });

    window.onerror = function(message, file, line, col, error) {
        if (!isUnLoading) {
            let payload = {
                type: 'error',
                message: message,
                file: file,
                line: line,
                col: col,
                stack: error.stack,
                user_agent: navigator.userAgent,
                url: window.location.href
            };

            let xhr = new XMLHttpRequest();
            xhr.open('POST', '/log-client-event', true);
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send(JSON.stringify(payload));

            // Show an alert if this is a form submit.
            // We recognize a form submit by some element having the class "masked"
            // or if either "submitFunction" or "doValidations" is in the stack trace.
            var hasMasked = document.querySelectorAll("form.masked").length > 0;
            if (hasMasked || (payload.stack && payload.stack.match(/(submitFunction|doValidations)/))) {
                alert("Sorry, something went wrong.\n\n(" + message + ")");
                try {
                    $("form.masked").unmask();
                } catch (e) {}
            }
        }

        if (oldOnError) {
            return oldOnError(message, file, line, col, error);
        } else {
            return false;
        }
    };
})();
