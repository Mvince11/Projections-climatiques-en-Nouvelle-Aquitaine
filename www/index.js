$(document).ready(function() {
            var $fonciere = $('#fonciere');
            var fonciereOffset = $fonciere.offset().top;

            $(window).scroll(function() {
                var scroll = $(window).scrollTop();
                var windowHeight = $(window).height();
                var fonciereVisible = scroll + windowHeight >= fonciereOffset;

                if (fonciereVisible) {
                    $fonciere.addClass('durete_fonciere');
                } else {
                    $fonciere.removeClass('durete_fonciere');
                }
            });
        });