{% javascript %}
function queueCountInfo(feedbackSelector, invokeSelector) {
    "use strict";

    var RETRIES = 5,
        RETRY_REPEAT_MS = 1000,
        UPDATE_REPEAT_MS = 1000,

        $feedback = $(feedbackSelector),
        $btn = $(invokeSelector),
        isUpdating = false,
        retryIvalId,
        retryCount = RETRIES,

        fetchAdminInfo,
        requestUpdate,
        updateFeedback;

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
            $feedback.addClass("label label-info").text("{_ Pivot queue count size: _}" + " " + queueCount);
            setTimeout(requestUpdate, UPDATE_REPEAT_MS);
        } else {
            if (isUpdating) {
                isUpdating = false;
                $feedback.fadeOut("slow");
                $btn.removeAttr("disabled");
                z_growl_add("{_ Search indices rebuilt. _}");
            }
        }
    };

    $btn.attr("disabled", "disabled");
    
    // it make take a short while before the info is available
    // so we try a couple of times
    retryIvalId = setInterval(function () {
        if (isUpdating || retryCount === 0) {
            clearInterval(retryIvalId);
        } else {
            requestUpdate();
            retryCount = retryCount - 1;
        }
    }, RETRY_REPEAT_MS);
}
{% endjavascript %}