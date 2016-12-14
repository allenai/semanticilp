$(document).ready(function(){
    answerCollectionRadioButton();
    knowledgeRadioButton();
    solverTypeSelector();
});

function visualizationBratWithLog(solverLogJson){
    $("svg").remove();
    if($('#brat-visualization').length == 0) {
        $("#brat-container").append('<svg width="100" height="100" id="brat-visualization"></svg>');
    }
    var text = solverLogJson.overalString.replace(/\|/g, '\n') + "                                  ";
    var entities = solverLogJson.entities;
    var relations = solverLogJson.relations;

    console.log("Applying the embed method ");
    // brat-visualization
    Util.embed("brat-visualization",
        {entity_types: [], relation_types: []},
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
