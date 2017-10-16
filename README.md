# TextILP 

A structured question answering system on top of free-form text, and borrows ideas from [TableILP](ai2-website.s3.amazonaws.com/publications/tableilp_ijcai_2016.pdf) and [TupleILP](https://arxiv.org/abs/1704.05572). 
The details of the system is explained in the following paper: 

```
@article{clark2016my,
  title={Question Answering as Global Reasoning over Semantic Abstractions},
  author={Khashabi, Daniel and Khot, Tushar and Sabharwal, Ashish and Roth, Dan},
  journal={????},
  year={2018}
}
```

## Running the annotators 
The system is dependant on the set of annotators provided in CogCompNLP. In order to run annotators, download version 

The system is tested with [v3.1.22 of CogCompNLP](https://github.com/CogComp/cogcomp-nlp/releases/tag/v3.1.22). 
Download the package and run the annotator servers: 

``` 
# running the main annotators 
./
# running github external annotators 
```


## Running the visual interface to the solver 
```
 > sbt 
 > project viz 
 > run 
```

To stop it, just do Ctrl+D. 
