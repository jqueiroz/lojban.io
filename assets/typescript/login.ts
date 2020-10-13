import * as $ from "jquery";

$(document).ready(function() {
    // TODO: prevent submitting the form with an empty handle
    $("#new-handle-random").click(function() {
        $(this).select();
    });
});
