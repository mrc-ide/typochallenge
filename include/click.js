$("#challenge_date").keydown(function(event) {
    if (event.keyCode === 13) {
        if (bowser.msie == true) {
            var do_submit = function() {
                $("#challenge_submit").click();
            };
            setTimeout(do_submit, 200);
        } else  if (bowser.firefox == true) {
            var do_submit = function() {
                $("#challenge_submit").click();
            };
            setTimeout(do_submit, 50);
        } else {
            $("#challenge_submit").click();
        }
    }
});

Shiny.addCustomMessageHandler('focus_challenge_date', function(message) {
    $('#challenge_date').select();
});
