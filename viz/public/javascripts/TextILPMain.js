$(document).ready(function(){
    answerCollectionRadioButton();
    knowledgeRadioButton();
    solverTypeSelector();
    loadSquadQuestionSamples();
});


var loadSquadQuestionSamples = function () {

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

};

// [B](f: (A) ⇒ [B]): [B]  ; Although the types in the arrays aren't strict (:
Array.prototype.flatMap = function(lambda) {
    return Array.prototype.concat.apply([], this.map(lambda));
};

function visualizationBratWithLog(solverLogJson){
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
    var text = solverLogJson.overalString.replace(/\|/g, '\n') + "                                  ";
    var entities = solverLogJson.entities;
    var relations = solverLogJson.relations;

    // console.log(entities);
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
