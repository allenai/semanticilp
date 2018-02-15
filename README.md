# SemanticILP 

A structured question answering system on top of free-form text, and borrows ideas from [TableILP](http://ai2-website.s3.amazonaws.com/publications/tableilp_ijcai_2016.pdf) and [TupleILP](https://arxiv.org/abs/1704.05572). 
The details of the system is explained in the following paper: 

```bibtex 
@article{semanticilp2018aaai,
  title={Question Answering as Global Reasoning over Semantic Abstractions},
  author={Khashabi, Daniel and Khot, Tushar and Sabharwal, Ashish and Roth, Dan},
  journal={Conference of Association for the Advancement of Artificial Intelligence},
  year={2018}
}
```

## Testing out the system
Our system relies on a couple of annotators that are not publicly available. As a result you (if outside CogComp) 
cannot run our *full* system. **However,** we have created a smaller system which works with public annotators. 

### Initializing the annotators 
The system is dependant on the set of annotators provided in CogCompNLP. In order to run annotators, download version 

The system is tested with [v3.1.22 of CogCompNLP](https://github.com/CogComp/cogcomp-nlp/releases/tag/v3.1.22). 
Download the package and run the annotator servers, on two different ports `PORT_NUMBER1` and `PORT_NUMBER2`.  
```bash 
# running the main annotators 
./pipeline/scripts/runWebserver.sh  --port PORT_NUMBER1 
# running github external annotators 
./external/scripts/runExternalAnnotatorsWebserver.sh --port PORT_NUMBER2  
```

Also the system requires [Sahand annotator server](https://github.com/danyaljj/sahand/releases/tag/1.2.5). 
This project makes distributed representations available over network. Run it, after downloading it: 
 
```
> sbt 
> project server 
> run SAHAND_PORT
```

Then you have to set the ports in SemanticILP. Open [`Constants.scala`](src/main/scala/org/allenai/ari/solvers/textilp/utils/Constants.scala) and set the ports.   

**Note:** The annotators require good amount of memory: 
- CogComp-NLP pipeline takes up to 25GB
- CogComp-NLP external annotators takes up to 35GB
- Sahand takes less than 10GB

### Missing Dependencies 
Unfortunately some of our dependencies are not available publicly. But there is a hacky way to get around this issue. 
We have put these dependencies [here](https://drive.google.com/file/d/1eAcBoZOJ3GyB1Y_zcge_dRvILY6rJPFC/view?usp=sharing), which you have to put them in our ivy cache folder. 
In a typical machine this is where there should be located at: `~/.ivy2/cache/`.

### Running SemanticILP 
And next you have to run the solver itself. You can run the system under different models. 
Here are the different models you can use: 

- Best overall  
- Best elementary-school science 
- Best process bank 

To set the model, take a look at `Constants.scala`. 

In order to initialize the solver, you have the following options: 

- Using it programmatically 
- Using it over network  

Next subsections clarify each of the above items: 

*Note:* here are the memory requirements: 
- SemanticILP solver: minimum around 8GB 
- Annotation Server (CogComp): minimum around 17GB 
- Annotation Server (CogComp-external): minimum around 15GB 

#### Run the solver programmatically 
To run the solver, clone this project and run create a instance of the solve: 

```scala 
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils
import org.allenai.ari.solvers.textilp.solvers.TextILPSolver
import org.allenai.ari.solvers.textilp.utils.SolverUtils

val annotationUtils = new AnnotationUtils()
val textILPSolver = new TextILPSolver(annotationUtils, verbose = false, SolverUtils.params)
  
val question = "A decomposer is an organism that"
val options = Seq("hunts and eats animals", "migrates for the winter",
                           "breaks down dead plants and animals", "uses water and sunlight to make food")
val paragraph = "organisms that obtain energy by eating dead plant or animal matter. " +
                            "DECOMPOSER An organism that breaks down cells of dead plants and animals into simpler substances." +
                            "The plants use sunlight, carbon dioxide, water, and minerals to make food that sustains themselves and other organisms in the forest."                             
val (selected, statistics) = textILPSolver.solve(question, options, paragraph)
println(selected)
println(statistics)
```

You can also install it locally (`publish-local`) and use it as a maven/sbt/... dependency in your program.   


*Note:* If you see an error like this: 
```
Caused by: java.lang.UnsatisfiedLinkError: no jscip-0.1.linux.x86_64.gnu.opt.spx in java.library.path
```
this means that the solver does not recognize the ILP binary files (common to linux). In that case, add the path to 
 your binary files, to your `LD_LIBRARY_PATH` variable. 
```
export LD_LIBRARY_PATH=path_to_lib_folder/
```


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
http://SOLVER_DOMAIN:SOLVER_PORT/solveQuestion?question=QUESTION&options=ANSWERS&snippet=SNIPPET
```
where `SOLVER_DOMAIN` is the domain of on which you're running the solver, `SOLVER_PORT` is the port on which 
the solver is running, and `ANSWERS` is the set of candidate answers separated by `//`. 
To access the solver, without paragraphs, set `SNIPPET` to be empty and it will try to retrieve a paragraph using lucene. 

To stop it, just do Ctrl+D.

Note that you can access the system via a graphical interface too: 

```
http://SOLVER_DOMAIN:SOLVER_PORT
```


## Questions?

Sure! Create issues or email Daniel. Suggestions? send a pull-request. 
