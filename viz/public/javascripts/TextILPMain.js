$(document).ready(function(){
    answerCollectionRadioButton();
    knowledgeRadioButton();
    solverTypeSelector();
    loadSampleQuestion();
});


var loadSampleQuestion = function () {

    // SQuAD questions
    var questionParagraphPairs = [];
    $.getJSON("/assets/squad-small.json", function (content) {
        var questions = content.data[0].paragraphs.flatMap(function (i) {
            return i.qas.map(function (j) {
                return j.question
            })
        });

        questionParagraphPairs = content.data[0].paragraphs.flatMap(function (i) {
            return i.qas.map(function (j) {
                return [j.question, i.context]
            })
        });

        var selectContent = "<option disabled selected value> -- squad questions: select one -- </option>";
        questions.forEach(function (q, i) {
            selectContent = selectContent + "<option value=" + i + ">" + q + "</option>"
        });

        $('#squad-select').html(selectContent);
    });

    // set the select behavior
    $("#squad-select").change(function () {
        var selectedIndex = $(this).prop("selectedIndex") - 1;
        var question = questionParagraphPairs[selectedIndex][0];
        var snippet = questionParagraphPairs[selectedIndex][1];
        $("#questionString").val(question);
        $("#knowTextArea").val(snippet);
    });

    // Aristo questions
    var aristoQuestions = [];
    $.getJSON("/assets/regents-train.json", function (content) {
        var selectContent = "<option disabled selected value> -- aristo questions: select one -- </option>";
        content.forEach(function(q, i){
            selectContent = selectContent + "<option value=" + i + ">" + q.question + "</option>";
        });
        aristoQuestions = content.map(function(q, i){ return q.question; });
        $('#aristo-select').html(selectContent);
    });

    // set the select behavior
    $("#aristo-select").change(function () {
        var selectedIndex = $(this).prop("selectedIndex") - 1;
        var split = aristoQuestions[selectedIndex].split(/\([A-Z]\)/);
        var question = split[0];
        var options = split.slice(1).join(" // ");
        $("#questionString").val(question);
        $("#candidateTextArea").val(options);
        $("#knowTextArea").val("");
        $("#knowLucene").prop("checked", true);
    });

    // process bank questions
    var processBankQuestionParagraphPairs = [];
    $.getJSON("/assets/processBank-train.json", function (content) {
        var questions = content[0].flatMap(function(a){ return a.paragraphQuestions.map(function(b){ return b.question })}).sort();

        processBankQuestionParagraphPairs = content[0].flatMap(function(a){
            return a.paragraphQuestions.map(function(b){ return [b.question, a.paragraphText, b.answers] })
        }).sort(function(c, d){
            if( c[0] >= d[0] )
                return 1;
            else
                return -1;
        });

        var selectContent = "<option disabled selected value> -- process-bank questions: select one -- </option>";
        questions.forEach(function (q, i) {
            selectContent = selectContent + "<option value=" + i + ">" + q + "</option>"
        });

        $('#processbank-select').html(selectContent);
    });

    // set the select behavior
    $("#processbank-select").change(function () {
        var selectedIndex = $(this).prop("selectedIndex") - 1;
        var question = processBankQuestionParagraphPairs[selectedIndex][0];
        var snippet = processBankQuestionParagraphPairs[selectedIndex][1];
        var optionsArray = processBankQuestionParagraphPairs[selectedIndex][2];
        $("#questionString").val(question);
        $("#knowTextArea").val(snippet);
        var options = optionsArray.join(" // ");
        $("#candidateTextArea").val(options);
    });
};

//processbank-select

// [B](f: (A) ⇒ [B]): [B]  ; Although the types in the arrays aren't strict (:
Array.prototype.flatMap = function(lambda) {
    return Array.prototype.concat.apply([], this.map(lambda));
};

function visualizationBratWithLog(solverLogJson){

    //var solverLogJson = {"overalString":"Question: A student tosses a ball into the air Which force causes the ball to fall back to the ground|Paragraph: Gravity is the force which makes a thrown ball fall back to the ground .. Gravity causes a ball to fall to the ground after it is thrown into the air.. Gravity causes the ball to fall back to earth ..|Options:  (1)  gravity    (2)   magnetism    (3)   mechanical    (4)   friction","entities":[["T0","tosses",[[20,26]]],["T1","is thrown",[[228,237]]],["T2","a ball",[[27,33]]],["T3","a ball",[[190,196]]],["T4","the air",[[39,46]]],["T5","the air..",[[243,252]]],["T6","Which force",[[48,59]]],["T7","Gravity",[[175,182]]],["T8","causes",[[183,189]]],["T9","causes",[[60,66]]],["T10","the ball",[[67,75]]],["T11","to fall",[[76,83]]],["T12","to fall",[[197,204]]],["T13","the ground",[[92,102]]],["T14","the ground",[[208,218]]],["T15","gravity",[[328,338]]]],"relations":[["R0"," 0.02 ",[["  ","T0"],["  ","T1"]]],["R1"," 0.9 ",[["  ","T2"],["  ","T3"]]],["R2"," 0.9 ",[["  ","T4"],["  ","T5"]]],["R3"," 0.15 ",[["  ","T6"],["  ","T7"]]],["R4"," 0.04 ",[["  ","T6"],["  ","T8"]]],["R5"," 0.9 ",[["  ","T9"],["  ","T8"]]],["R6"," 0.9 ",[["  ","T10"],["  ","T3"]]],["R7"," 0.9 ",[["  ","T11"],["  ","T12"]]],["R8"," 0.9 ",[["  ","T13"],["  ","T14"]]],["R9"," 0.9 ",[["  ","T7"],["  ","T15"]]]],"explanation":""};

    // I don't know why we sometimes have new lines in the strings ugh
    solverLogJsonNew = JSON.parse(JSON.stringify(solverLogJson).replace(/\\n/, " "));

    var entity_types = [
        {
            "type": "  ",
            "labels": [ "  " ],
            "bgColor": "#7fa2ff",
            "borderColor": "darken"
        }
    ];

    var relation_types = [
        {
            "type": "  ",
            "labels": [ "  "],
            "dashArray": "3,3",
            "color": "purple"
        }
    ];

    $("svg").remove();
    if($('#brat-visualization').length == 0) {
        $("#brat-container").append('<svg width="100" height="100" id="brat-visualization"></svg>');
    }
    var text = solverLogJsonNew.overalString.replace(/\|/g, "\n") + "                                                               ";
    var entities = solverLogJsonNew.entities; // .slice(0, 40);
    var relations = solverLogJsonNew.relations;


    // console.log(text);
    // console.log(entities);
    // console.log(solverLogJsonNew.entities.length);
    // console.log(relations);

    console.log("Applying the embed method ");
    // brat-visualization
    Util.embed("brat-visualization",
        {entity_types: entity_types, relation_types: relation_types},
        {text: text, entities: entities, relations: relations}
    );
    console.log("done embedding");
}

var answerCollectionRadioButton = function(){
    $("#candidatesManual").change(function() {
            $("#candidateTextArea").prop("disabled", false);
        }
    );
    $("#candidateAuto").change(function() {
            $("#candidateTextArea").prop("disabled", true);
        }
    );
};

var knowledgeRadioButton = function(){
    $("#knowManual").change(function() {
            $("#knowTextArea").prop("disabled", false);
        }
    );
    $("#knowLucene").change(function() {
            $("#knowTextArea").prop("disabled", true);
        }
    );
};

var solverTypeSelector = function(){
    $("#selectTableILP").click(function() {
            $("#solver-dropdown").text("Solver: TableILP ")
        }
    );

    $("#selectSalience").click(function() {
            $("#solver-dropdown").text("Solver: Salience ")
        }
    );

    $("#selectLucene").click(function() {
            $("#solver-dropdown").text("Solver: Lucene ")
        }
    );
};
