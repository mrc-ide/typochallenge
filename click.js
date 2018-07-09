$("#challenge_date").keydown(function(event) {
    if (event.keyCode === 13) {
        var do_submit = function() {
            // console.log("submitting results");
            $("#challenge_submit").click();
        };
        // console.log("recieved enter");
        setTimeout(do_submit, 500);
    }
});

Shiny.addCustomMessageHandler('focus_challenge_date', function(message) {
    $('#challenge_date').select();
});
