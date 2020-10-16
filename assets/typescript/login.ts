import * as $ from "jquery";

function areAllCharactersValidForHandle(handle : string) {
    return handle.search(/[^A-Za-z0-9-_.]/) == -1;
}

$(document).ready(function() {
    $("#new-handle-random").click(function() {
        $(this).select();
    });

    $("input.handle").on("keypress", function(e) {
        return areAllCharactersValidForHandle(e.key);
    });

    $(".login-main form").on("submit", function(e) {
        let handle = $(this).children("input.handle").val();
        if (!handle) {
            alert("The handle must not be empty.");
            return false;
        }
    });
});
