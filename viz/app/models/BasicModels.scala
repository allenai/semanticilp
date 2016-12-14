package models

case class QuestionChoice(str: String)

case class TextSnippet(str: String)

case class Question(str: String, questionChoice: String, snippet: TextSnippet)

case class PreFilledQuestions(questions: Seq[Question])

sealed trait SolverType {}
case object TextILP extends SolverType
case object Salience extends SolverType
case object Lucene extends SolverType

sealed trait CandidateGeneration {}
case object CandidatesGiven extends CandidateGeneration
case object CandidatesAutomatic extends CandidateGeneration

sealed trait ExternalKnowledge {}
case object KnowledgeGiven extends ExternalKnowledge
case object LuceneKnowledge extends ExternalKnowledge

case class FormContent(
  solverType: SolverType,
  preFilledQuestions: PreFilledQuestions,
  preFilledQuestionIndOpt: Option[Int],
  questionOpt: Option[Question],
  candidateGeneration: CandidateGeneration,
  externalKnowledge: ExternalKnowledge,
  solverLog: String
)

object StaticContent {
  val preFilledQuestions = PreFilledQuestions(
    Seq(
      Question(
        "What was Nikola Tesla's ethnicity?",
        "Serbian // African // American // Asian",
        TextSnippet("Nikola Tesla (Serbian Cyrillic: Никола Тесла; 10 July 1856 – 7 January 1943) was a Serbian American inventor, electrical engineer, mechanical engineer, physicist, and futurist best known for his contributions to the design of the modern alternating current (AC) electricity supply system.")
      ),
      Question(
        "A decomposer is an organism that",
        "hunts and eats animals // migrates for the winter // breaks down dead plants and animals // uses water and sunlight to make food",
        TextSnippet("explanation:Decomposers: organisms that obtain energy by eating dead plant or animal matter. " +
          "Windy, cloudy, rainy, and cold are words that help describe\tfocus: deposition. " +
          "explanation:DECOMPOSER An organism that breaks down cells of dead plants and animals into simpler substances." +
          "explanation:The plants use sunlight, carbon dioxide, water, and minerals to make food that sustains themselves and other organisms in the forest.")
      ),
      Question("Windy, cloudy, rainy, and cold are words that help describe",
        "evaporation // deposition // matter // weather",
        TextSnippet("explanation:The words rainy , cloudy windy and cold are of course adjectives which we use to " +
          "describe the weather. explanation:He's out there seven days a week no matter how hot, how cold, how windy, rainy ")),
      Question("Which force causes rocks to roll downhill?", "gravity // friction // rosion // magnetism",
        TextSnippet("explanation:The cause of the force of magnetism does not reside in the magnet.explanation:The erosion of material a t the site of seepage causes rock and debris on the slope above this area to collapse and slide downhill, creating the alcove.explanation:Friction is one force causes a ball to roll downhill .explanation:The force of gravity, which holds us to the Earth, causes eroded rocks to roll off elevated areas into valleys and river beds.")),
      Question("Which form of energy is found in food?", "chemical // electrical // sound // mechanical",
        TextSnippet("explanation:Energy can be found in the form of electric, mechanical, internal energy, etc.explanation:Discuss ways in which sound energy is converted to other forms of energy, or other forms of energy are changed to sound energy.explanation:This is not the same as electrical-energy which is a form of external-energy.explanation:Food energy is a form of chemical energy.")),
      Question("Which two observations are both used to describe weather?",
        "precipitation and runoff // temperature and sky conditions // wind speed and erosion // types of clouds and deposition",
        TextSnippet("explanation:Describe the two types of weather satellites used to observe clouds.explanation:Measuring wind Two features of wind, its speed and its direction, are used in describing and forecasting explanation:ASOS will provide weather observations which include: temperature, dew point, wind, altimeter setting, visibility, sky condition, /and precipitation. explanation:The general form of the multiple linear regression models used to predict the runoff is an equation in which the estimate of runoff for the remainder of the water year is a function of antecedent runoff, seasonal precipitation to date, and observed snow water content.")),
      Question("Which physical structure would best help a bear to survive a winter in New York State?",
        "big ears // brown eyes // black nose // thick fur",
        TextSnippet("explanation:A thick coat of white fur helps bears survive in these latitudes.explanation:Hibernation is an adaptation that helps black bears survive the winter.explanation:Staring eye-to-eye into the face of winter, the New York Yankees would not blink.explanation. Adirondack Hunting Adirondack Deer and Bear Hunting Experience the best of Adirondack Mountain big game hunting in New York State.")),
      Question("Which sense can be used to determine an object’s ability to reflect light?",
        "sight // hearing // smell // taste ",
        TextSnippet("explanation:A study of the relation between physical exercise and learning ability Is audio or visual information better remembered Study the \"comfort zone\" different people have, how the comfort zone varies between sexes, and between friends and strangers Determine if there is a difference between various groups in overcoming visual illusions to determine what is really there Determine if males and females have different abilities in estimating an object's size Determine how well people identify foods using only the sense of smell Determine if people can identify the original scents used to make homemade fragrances and perfumes Determine if smells, odors or scents affect peoples' mood.explanation:The animal hears the echoes, and uses them to determine the direction and distance to the reflecting object.explanation:Some objects of sight which in light are invisible, in darkness stimulate the sense;explanation:Using taste or smell to determine objects")),
      Question("Green plants get the energy they need to make food from", "air // soil // sunlight // water ",
        TextSnippet("explanation:Plants make their own food from carbon dioxide and water, but the energy needed for this food-making process is light.explanation:Because they lack chlorophyll (the green pigment that absorbs energy from sunlight and uses it to make food), mushrooms get the food energy they need in other ways.explanation:Plant nutrients are chemicals in the soil that plants need, e.g. plant nutrients are needed to make the green colour that absorbs the sunlight energy.explanation:Chlorophyll makes it possible for green plants to capture the energy of sunlight and use it to make food (sugars and starches) from air and water.")),
      Question("Which structure of a bird is correctly paired with its function?",
        "claws for obtaining food // eyes for growing // wings for eliminating waste // feathers for breathing",
        TextSnippet("explanation:On each chromosome pair are smaller bodies (genes) which control all aspects of a bird, its colour, shape, bone structure, length of feather, size etc.explanation:That resemblance of structures which depends upon similarity of function, as in the wings of insects and birds.explanation:Diseases and Function of the Eye - 2 credits A course which introduces students to: a) the structural parts of the eye and its functions;explanation:cheliped (see appendages) The first pair of legs, carries the large claw which is used for defense and obtaining food."))
    )
  )

  val sampleFormContent = FormContent(
    Lucene,
    preFilledQuestions,
    None,
    Some(preFilledQuestions.questions.head),
    CandidatesAutomatic,
    LuceneKnowledge,
    ""
  )

  val initialFormContent = FormContent(
    TextILP,
    preFilledQuestions,
    None,
    Some(Question("", "", TextSnippet(""))),
    CandidatesGiven,
    KnowledgeGiven,
    ""
  )

  def getContentWithPrefilled(index: Int): FormContent = {
    initialFormContent.copy(questionOpt = Some(preFilledQuestions.questions(index)))
  }
}
