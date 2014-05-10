{% javascript %}
function queueCountInfo(feedbackSelector, invokeSelector) {
    "use strict";

    var RETRIES = 10,
        RETRY_REPEAT_MS = 3000,
        UPDATE_REPEAT_MS = 1000,
        COUNT_SIZE_MSG = "{_ Pivot queue count size: _}",
        
        $feedback = $(feedbackSelector),
        $btn = $(invokeSelector),
        isUpdating = false,
        retryIvalId,
        retryCount = RETRIES,

        setFeedback,
        fetchAdminInfo,
        requestUpdate,
        updateFeedback,
        startTrying,
        resetUI,
        initUI;

    setFeedback = function (message, count) {
        var msg = message;
        if (count !== undefined) {
            msg += " " + count;
        }
        $feedback.text(msg);
    };

    fetchAdminInfo = function (callback) {
        $.ajax({
            dataType: "json",
            type: "get",
            url: "/api/admin/info"
        }).done(function (data, textStatus, jqXHR) {
            if (data !== undefined) {
                callback(data);
            }
        }).fail(function (jqXHR, textStatus, errorThrown) {
            if (console) {
                console.log("fetchQueueCount JSON request failed");
            }
        });
    };

    requestUpdate = function () {
        fetchAdminInfo(function (data) {
            if (data) {
                if (data.pivot_queue_count !== undefined) {
                    updateFeedback(data.pivot_queue_count);
                }
            }
        });
    };

    updateFeedback = function (count) {
        var queueCount = parseInt(count, 10);
        if (queueCount > 0) {
            isUpdating = true;
            setFeedback(COUNT_SIZE_MSG, queueCount);
            setTimeout(requestUpdate, UPDATE_REPEAT_MS);
        } else {
            if (isUpdating) {
                isUpdating = false;
                setFeedback(COUNT_SIZE_MSG, 0);
                z_growl_add("{_ Search indices rebuilt. _}");

                // wait a little bit
                setTimeout(function() {
                    $feedback.fadeOut("slow");
                    $btn.removeAttr("disabled");
                }, 1500);
            }
        }
    };
    
    // it make take a short while before the info is available
    // so we try a couple of times
    startTrying = function() {
        retryIvalId = setInterval(function () {
            if (isUpdating) {
                clearInterval(retryIvalId);
            } else if (retryCount === 0) {
                clearInterval(retryIvalId);
                resetUI();
            } else {
                requestUpdate();
                retryCount = retryCount - 1;
            }
        }, RETRY_REPEAT_MS);
    };

    resetUI = function () {
        $btn.removeAttr("disabled");
        $feedback.hide("slow");
    };
    
    initUI = function () {
        $btn.attr("disabled", "disabled");
        $feedback.addClass("label label-info");
        setFeedback("{_ Retrieving status... _}");
        startTrying();
    };
    
    initUI();
}
{% endjavascript %}