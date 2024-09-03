
$(window).scroll(function() {   
var scroll = $(window).scrollTop();
if (scroll >= 1200) {
$("#fonciere").addClass("durete_fonciere");
} else {
$("#fonciere").removeClass("durete_fonciere");
}
});
