{% javascript %}
function queueCountInfo(feedbackSelector) {
    "use strict";

    var RETRIES = 10,
        RETRY_REPEAT_MS = 3000,
        UPDATE_REPEAT_MS = 1000,
        COUNT_SIZE_MSG = "{_ Pivot queue count size: _}",

        $feedback = $(feedbackSelector),
        isUpdating = false,
        retryInvalId,
        retryCount = RETRIES,

        setFeedback,
        requestUpdate,
        updateFeedback,
        startTrying,
        resetUI,
        initUI;

    setFeedback = function (message, backlog, total) {
        var msg = message;
        if (backlog !== undefined) {
            msg += " " + backlog;
        }
        if (total !== undefined && total > 0) {
            msg += " (" + (total - backlog) + ")";
        }
        $feedback.text(msg);
    };

    requestUpdate = function () {
        cotonic.broker.call("$promised/bridge/origin/model/admin/get/pivot_queue_count", {})
            .then(function(msg) {
                if (msg.payload.status == 'ok') {
                    updateFeedback(msg.payload.result);
                }
            });
    };

    updateFeedback = function (result) {
        let queueCount = parseInt(result.backlog, 10);
        let queueTotal = parseInt(result.total, 10);
        if (queueCount > 0) {
            isUpdating = true;
            setFeedback(COUNT_SIZE_MSG, queueCount, queueTotal);
            setTimeout(requestUpdate, UPDATE_REPEAT_MS);
        } else {
            setFeedback(COUNT_SIZE_MSG, 0, queueTotal);
            clearInterval(retryInvalId);
            retryInvalId = undefined;
            if (isUpdating) {
                isUpdating = false;
                z_growl_add("{_ Search indices rebuilt. _}");
            }
        }
    };

    // it make take a short while before the info is available
    // so we try a couple of times
    startTrying = function() {
        if (retryInvalId) {
            clearInterval(retryInvalId);
        }
        retryInvalId = setInterval(function () {
            if (isUpdating) {
                clearInterval(retryInvalId);
            } else if (retryCount === 0) {
                clearInterval(retryInvalId);
                resetUI();
            } else {
                requestUpdate();
                retryCount = retryCount - 1;
            }
        }, RETRY_REPEAT_MS);
    };

    resetUI = function () {
        $feedback.hide("slow");
    };

    initUI = function () {
        $feedback.addClass("label label-info");
        setFeedback("{_ Retrieving status... _}");
        startTrying();
    };

    initUI();
}
{% endjavascript %}
