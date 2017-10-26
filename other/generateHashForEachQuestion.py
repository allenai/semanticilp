## I used this because I need to convert each question to a hash value

import csv
import hashlib

fourth = False

if fourth:
    questions_file = "/Users/daniel/ideaProjects/TextILP/other/questionSets/Public-Feb2016-Elementary-NDMC-Test.tsv"
    lucene_predictions = "/Users/daniel/ideaProjects/TextILP/other/lucene-public-test-4th.csv"
    textilp_predictions = "/Users/daniel/ideaProjects/TextILP/other/predictionPerQuestion-public-8thgrade-test-40-questions.txt"
else:
    questions_file = "/Users/daniel/ideaProjects/TextILP/other/questionSets/Public-Gr08-Test.tsv"
    lucene_predictions = "/Users/daniel/ideaProjects/TextILP/other/lucene-public-test-8th3.csv"
    textilp_predictions = "/Users/daniel/ideaProjects/TextILP/other/predictionPerQuestion-public-4thgrade-test-40-questions.txt"

lucene_selected_per_hash = {}
lucene_score_per_hash = {}
question_to_hash_map = {}
hash_to_questin_map = {}
textilp_score_per_question = {}
textilp_selected_per_question = {}
list_of_questions = []

def get_complete_question(q_only):
    print(len(list_of_questions))
    for q in list_of_questions:
        if q_only in q:
            return q
    print("didn't find .. . . " + q_only)
    return ""

def main():
    with open(questions_file) as tsvfile:
        csvreader = csv.reader(tsvfile, delimiter="\t")
        for line in csvreader:
            #print(line[0])
            list_of_questions.append(line[0])

    with open(lucene_predictions) as csvfile:
        csvreader = csv.reader(csvfile, delimiter=",")
        for line in csvreader:
            print(line)
            print(line[2])
            print(line[8])
            if(fourth):
                if(line[2] == '1.0'):
                    lucene_selected_per_hash[line[0]] = line[1]
                    lucene_score_per_hash[line[0]] = (line[2] == '1.0' and line[8] == '1')
            else:
                if(line[2] == '1'):
                    lucene_selected_per_hash[line[0]] = line[1]
                    lucene_score_per_hash[line[0]] = (line[2] == '1' and line[3] == '1')


    with open(textilp_predictions) as tsvfile:
        tsvreader = csv.reader(tsvfile, delimiter="\t")
        for line in tsvreader:
            #print(line)
            full_question = get_complete_question(line[0]) # .encode("utf-8")
            #print(full_question)
            hash = hashlib.sha1(full_question.encode("utf-8")).hexdigest()[:8]
            question_to_hash_map[line[0]] = hash
            hash_to_questin_map[hash] = line[0]
            textilp_selected_per_question[line[0]] = line[1]
            textilp_score_per_question[line[0]] = line[1]==line[2]
            #print(hashlib.sha1(line[0].encode("utf-8")).hexdigest()[:8])

    print(len(lucene_selected_per_hash))
    print(len(lucene_score_per_hash))
    print(len(textilp_score_per_question))
    print(len(hash_to_questin_map))
    print(len(question_to_hash_map))

    # for each question, check whether the hash for lucene prediction exists or not, and if so, calculate the overlap
    both_correct = 0
    both_incorrect = 0
    textilp_correct_only = 0
    lucene_correct_only = 0
    textilp_correct = 0
    lucene_correct = 0
    total = 0
    print(lucene_score_per_hash)
    for q, hash in question_to_hash_map.items():
        if hash in lucene_score_per_hash:
            total = total + 1
            textilp_score = textilp_score_per_question[q]
            lucene_score = lucene_score_per_hash[hash]
            print(textilp_score)
            print(lucene_score)
            if lucene_score:
                lucene_correct = lucene_correct + 1
            if textilp_score:
                textilp_correct = textilp_correct + 1
            if textilp_score and lucene_score:
                both_correct = both_correct + 1
            elif not textilp_score and lucene_score:
                lucene_correct_only = lucene_correct_only + 1
            elif not lucene_score and textilp_score:
                textilp_correct_only = textilp_correct_only + 1
            else:
                both_incorrect = both_incorrect + 1
        else:
            print("hash does not exist in the map ")
            print(hash)

    print(both_correct)
    print(lucene_correct_only)
    print(textilp_correct_only)
    print(both_incorrect)
    print(total)
    print("textilp: " + str(textilp_correct / total))
    print("lucene: " + str(lucene_correct / total))

    import matplotlib.pyplot as plt
    #
    # Data to plot
    labels = 'Both\nCorrect', 'Lucene\n correct only', 'SemanticILP\ncorrect only', 'None correct'
    sizes = [both_correct, lucene_correct_only, textilp_correct_only, both_incorrect]
    colors = ['gold', 'yellowgreen', 'lightcoral', 'lightskyblue']
    explode = (0.0, 0, 0, 0)  # explode 1st slice

    # Plot
    plt.pie(sizes, explode=explode, labels=labels, colors=colors,
            autopct='%1.1f%%', shadow=False, startangle=0)

    plt.axis('equal')
    if(fourth):
        plt.xlabel('Overlap of the predictions, on AI2Public-4th')
    else:
        plt.xlabel('Overlap of the predictions, on AI2Public-8th')
    plt.show()

if __name__ == '__main__':
    main()

