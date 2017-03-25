package org.allenai.ari.solvers.textilp.utils

import edu.illinois.cs.cogcomp.annotation.Annotator
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, SpanLabelView, TextAnnotation }
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.ResourceManager
import org.allenai.ari.controller.questionparser.{ FillInTheBlankGenerator, QuestionParse }
import org.apache.commons.codec.digest.DigestUtils
import org.mapdb.{ DBMaker, HTreeMap, Serializer }

class FillInBlankAnnotator extends Annotator("FillInBlankGenerator", Array.empty) {
  private val db = DBMaker.fileDB("fillInBlankGenerator.cache").closeOnJvmShutdown().transactionEnable().make()
  lazy val fitbGenerator = FillInTheBlankGenerator.mostRecent
  override def initialize(rm: ResourceManager): Unit = {
    // do nothing
  }

  override def addView(ta: TextAnnotation): Unit = {
    val concurrentMap: HTreeMap[String, Array[Byte]] = db.hashMap(viewName, Serializer.STRING, Serializer.BYTE_ARRAY).createOrOpen()
    val key = DigestUtils.sha1Hex(ta.text)
    if (concurrentMap.containsKey(key)) {
      val taByte: Array[Byte] = concurrentMap.get(key)
      val cachedTa = SerializationHelper.deserializeTextAnnotationFromBytes(taByte)
      ta.addView(viewName, cachedTa.getView(viewName))
    } else {
      val qparse = QuestionParse.constructFromString(ta.text)
      val fitbQuestionStrOpt = fitbGenerator.generateFITB(qparse).map(_.text)
      val cons = new Constituent(fitbQuestionStrOpt.getOrElse(""), viewName, ta, 0, ta.getTokens.length)
      val vu = new SpanLabelView(viewName, ta)
      vu.addConstituent(cons)
      ta.addView(viewName, vu)
      if (concurrentMap != null) {
        concurrentMap.put(key, SerializationHelper.serializeTextAnnotationToBytes(ta));
        this.db.commit();
      }
    }
  }

  def close = db.close()
}
