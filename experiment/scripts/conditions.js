/* set up list of conditions with wordings */ 

function addCondition(condName, wording) {
	function Condition(condName, wording) {
		this.condName = condName;
		this.wording = wording;
	};
	newCondition = new Condition(condName, wording);
	conditions[newCondition.condName] = newCondition;
};

conditions = {};
addCondition("getting hungry", "getting hungry");
addCondition("experiencing pain", "experiencing pain");
addCondition("feeling tired", "feeling tired");
addCondition("experiencing fear", "experiencing fear");
addCondition("doing computations", "doing computations");
addCondition("experiencing pleasure", "experiencing pleasure");
addCondition("being conscious", "being conscious");
addCondition("having free will", "having free will");
addCondition("feeling safe", "feeling safe");
addCondition("having desires", "having desires");
addCondition("feeling calm", "feeling calm");
addCondition("feeling nauseated", "feeling nauseated");
addCondition("getting angry", "getting angry");
addCondition("having intentions", "having intentions");
addCondition("being self-aware", "being self-aware");
addCondition("Detecting odors", "detecting odors");
addCondition("feeling embarrassed", "feeling embarrassed");
addCondition("experiencing pride", "experiencing pride");
addCondition("feeling love", "feeling love");
addCondition("experiencing guilt", "experiencing guilt");
addCondition("feeling disrespected", "feeling disrespected");
addCondition("feeling depressed", "feeling depressed");
addCondition("holding beliefs", "holding beliefs");
addCondition("understanding how others are feeling", "understanding how others are feeling");
addCondition("experiencing joy", "experiencing joy");
addCondition("having a personality", "having a personality");
addCondition("feeling happy", "feeling happy");
addCondition("telling right from wrong", "telling right from wrong");
addCondition("having thoughts", "having thoughts");
addCondition("exercising self-restraint", "exercising self-restraint");
addCondition("remembering things", "remembering things");
addCondition("recognizing someone", "recognizing someone");
addCondition("sensing temperatures", "sensing temperatures");
addCondition("communicating with others", "communicating with others");
addCondition("working toward a goal", "working toward a goal");
addCondition("perceiving depth", "perceiving depth");
addCondition("detecting sounds", "detecting sounds");
addCondition("seeing things", "seeing things");
addCondition("making things", "making things");
addCondition("reasoning about things", "reasoning about things");

// set up button behaviors for surveys slide

$('.slide#surveys button').click(function() { // select condition
	experiment.newData.condition = surveysSlide.condition.condName;
	experiment.newData.wording = surveysSlide.condition.wording;
	experiment.next();
	window.scrollTo(0, 0);
});

// set up how to display surveys slide

var surveysSlide = {
	list: Object.keys(conditions).map(function (key) {return conditions[key]}),
	order: [],
	condition: ""
}

surveysSlide.condition = randomElementNR(surveysSlide.list);

$('.slide#surveys span#survey-descrip1').text(surveysSlide.condition.condName)
$('.slide#surveys span#survey-descrip2').text(surveysSlide.condition.wording);