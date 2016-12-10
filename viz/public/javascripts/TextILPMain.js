$(document).ready(function(){
    installSubmitClickEvent();
    answerCollectionRadioButton();
    knowledgeRadioButton();
    solverTypeSelector();
});

var installSubmitClickEvent = function() {
    $("#submitBtn").click(function() {
        var solverType = $("#solver-dropdown").prop("text");
        var questionStr = $("#questionString").val();
        var options = $("#candidateTextArea").val();
        var snippet = $("#knowTextArea").val();
        $.ajax({
            type: 'POST',
            url: "/solve",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            data: JSON.stringify({
                "solverType": solverType,
                "question": questionStr,
                "options": options,
                "snippet": snippet
            }),
            success: function(response) {
                console.log("sent ... ");
            },
            error: function(xhr) {
                console.log("failed ... ");
            }
        });
    });
};

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
