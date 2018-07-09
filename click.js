$("#challenge_date").keydown(function(event) {
    if (event.keyCode === 13) {
        if (bowser.msie == true) {
            var do_submit = function() {
                $("#challenge_submit").click();
            };
            setTimeout(do_submit, 500);
        } else {
            $("#challenge_submit").click();
        }
    }
});

Shiny.addCustomMessageHandler('focus_challenge_date', function(message) {
    $('#challenge_date').select();
});
