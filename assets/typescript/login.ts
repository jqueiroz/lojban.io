import * as $ from "jquery";

$(document).ready(function() {
    // TODO: prevent submitting the form with an empty handle
    // TODO: enforce that the handle contains only valid characters
    $("#new-handle-random").click(function() {
        $(this).select();
    });
});
