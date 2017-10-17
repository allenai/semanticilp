# SemanticILP 

A structured question answering system on top of free-form text, and borrows ideas from [TableILP](ai2-website.s3.amazonaws.com/publications/tableilp_ijcai_2016.pdf) and [TupleILP](https://arxiv.org/abs/1704.05572). 
The details of the system is explained in the following paper: 

```bibtex 
@article{clark2016my,
  title={Question Answering as Global Reasoning over Semantic Abstractions},
  author={Khashabi, Daniel and Khot, Tushar and Sabharwal, Ashish and Roth, Dan},
  journal={Conference of Association for the Advancement of Artificial Intelligence},
  year={2018}
}
```

## Testing out our system
Our system relies on a couple of annotators that are not publicly available. As a result you (if outside CogComp) 
cannot run our *full* system. **However,** we have created a smaller system which works with public annotators. 

### Initializing the annotators 
The system is dependant on the set of annotators provided in CogCompNLP. In order to run annotators, download version 

The system is tested with [v3.1.22 of CogCompNLP](https://github.com/CogComp/cogcomp-nlp/releases/tag/v3.1.22). 
Download the package and run the annotator servers, on two different ports `PORT_NUMBER1` and `PORT_NUMBER2`.  
```bash 
# running the main annotators 
./external/scripts/runWebserver.sh  --port PORT_NUMBER1 
# running github external annotators 
./external/scripts/runExternalAnnotatorsWebserver.sh --port PORT_NUMBER2  
```

Then you have to set the ports in SemanticILP. Open `Constants.scala` and set the ports.   

### Running SemanticILP 

And next you have to run the solver itself. You can run the system under different models. 
Here are the different models you can use: 

- Best overall  
- Best elementary-school science 
- Best process bank 

To set the model, take a look at [Constants](Constants.scala). 


In order to initialize the solver, you have the following options: 

- Using it programmatically 
- Using it over network  

Next subsections clarify each of the above items: 

#### Run the solver programmatically 
To run the solver, clone this project and run create a insance of the solve: 

```scala 

```

You can also install it locally (`publish-local`) and use it as a maven/sbt/... dependency in your program.   

#### Run the solver over a network 
This is for the case where you want to access the system either: 
 - Limited memory, not enough to run the system on your machine 
 - Need to access from a programming language, other than Scala (or any other JVM-based language)
 - Multiple people trying to use it at the same time. 
 
To the run the system over the network, run the following script: 
```
 > sbt 
 > project viz 
 > run 
```

And access it in this URL: 
```bash
http://SOLVER_DOMAIN:SOLVER_PORT/solveWithParagraph?paragraph=...&question=....&candidates=...
```
where `SOLVER_DOMAIN` is the domain of on which you're running the solver, and `SOLVER_PORT` is the port on which 
the solver is running. To access the solver, without paragraphs, try this:
```bash
http://SOLVER_DOMAIN:SOLVER_PORT/solveWithoutParagraph?question=....&candidates=...
```
To stop it, just do Ctrl+D.

Note that you can access the system via a graphical interface too: 

```
http://SOLVER_DOMAIN:SOLVER_PORT
```


## Questions?

Sure! Create issues or email Daniel. Suggestions? send a pull-request. 