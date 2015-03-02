/* set up helper functions */

// define function to show a slide
function showSlide(id) { 
	console.log(id); // show which slide is being presented
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

/* set up button behaviors */

$('.slide#preview button').click(function() {
	if(turk.previewMode === false) {
		window.open('experiment.html');
	}
})


/* set up how to display preview slide */
var previewSlide = {
	showPreview: function() {
		if (turk.previewMode === true) {
			$('.slide #preview-text').text("You must accept the HIT before continuing to the study.");
			$('.slide button').hide();
			showSlide("preview");
		} else if (UTWorkerLimitReached(ut_id)) {
			$('.slide #preview-text').text("You have already completed this survey, or something very similar. Please return this HIT to allow someone else to participate. Thanks for your interest - we hope to see you again next time!");
			$('.slide #preview-text').css({ "color": "darkred", "font-weight": "bold", "width": "80%", "margin": "auto"});
			$('.slide button').hide();
			showSlide("preview");
		} else {
			$('.slide #preview-text').text("Click the 'Next' button to begin! The study will open in a new window.");
			$('.slide button').text("Next");
			showSlide("preview");
		}
	}
}

// create unique turker id
var ut_id = "ggw-rep01";

// show preview slide
previewSlide.showPreview();