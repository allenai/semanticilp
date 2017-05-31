This dataset consists of 200 paragraphs that describe biological processes.
Each paragraph is annotated with its process structure, and accompanied by a
few multiple-choice questions about the process. Each question has two
possible answers of which exactly one is correct.

The dataset contains three files:
1. bioprocess-bank-questions.tar.gz: There is an xml file for each paragraph
containing the paragraph ID, the questions and answers
2. process-bank-structures-train.tar.gz: These are the structure annotations
used for training our structure predictor. Each paragraph has two files - one containing the
text and one containing the annotation. This is standard BRAT format
(http://brat.nlplab.org/).
3. process-bank-structures-test.tar.gz - Therea are structure annotations used
for testing. They are also in BRAT format.
