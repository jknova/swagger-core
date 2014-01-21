package com.wordnik.swagger.converter

import com.wordnik.swagger.model._
import com.wordnik.swagger.core.SwaggerContext
import com.wordnik.swagger.core.util.ModelUtil

import org.slf4j.LoggerFactory

import scala.collection.mutable.{ ListBuffer, HashMap, LinkedHashMap }

object ModelInheritenceUtil {
  private val LOGGER = LoggerFactory.getLogger(ModelInheritenceUtil.getClass)
  def expand(models: Map[String, Model]): Map[String, Model] = {
    val submodels = new HashMap[String, Model]
    val baseModels = new HashMap[String, Model]
    val dependentModels = new HashMap[String, Model]


    // look at all models with baseModel set
    for((name, m) <- models) {
      LOGGER.debug("expanding " + name)
      if(m.baseModel != None) {
        LOGGER.debug("got a base model from " + name)
        // get the baseModel
        val typeRef = m.baseModel.get
        if(ModelUtil.shoudIncludeModel(typeRef)) {
          try{
            // load baseModel
            LOGGER.debug("loading " + typeRef)
            val cls = SwaggerContext.loadClass(typeRef)
            for(model <- ModelConverters.readAll(cls))
              if(typeRef == model.qualifiedType) { // if baseModel == model loaded
                LOGGER.debug("added baseModel " + typeRef)
                baseModels += model.name -> model
              }
              else {  // if model is a dependency of the baseModel
                LOGGER.debug("added dependentModel " + name)
                dependentModels += model.name -> model
              }
          }
          catch {
            case e: ClassNotFoundException => Map()
          }
        }
        else {
          LOGGER.debug("skipping " + typeRef)
        }
        /* 
         * The intent of this commented-out code is to remove model properties that are inherited from 
         * the base model.
         * 
         * However, the code is incorrect -- it checks all base models, not just the one for the
         * current model.
         * 
         * In addition, the code usually does not have any effect, since the current model will 
         * typically be added as a dependent model in the loop above. The dependent models do not 
         * have their inherited properties pruned, and they override the submodels in the concatenation
         * at the end of this method.
         * 
         * In any case, we actually do not want the inherited properties to be pruned. Swagger UI does
         * not have support for depicting inheritance, so it is better for the inherited properties to 
         * be listed as belonging to the submodel.
         
        // subtract fields from base models
        val basePropNames = baseModels.map(_._2.properties.keys).flatten.toSet
        LOGGER.debug("basePropNames: " + basePropNames)
        val propertyMap = new LinkedHashMap[String, ModelProperty]
        for((name, prop) <- m.properties) {
          LOGGER.debug("inspecting " + name)
          if(!basePropNames.contains(name)) propertyMap += name -> prop
        }
        submodels += name -> m.copy(properties = propertyMap)
        */
      }
      submodels += name -> m
    }
    submodels.map(m => (m._1, m._2.copy(baseModel = {
      m._2.baseModel match {
        case Some(e) => Some(ModelUtil.cleanDataType(m._2.baseModel.get))
        case _ => None
      }
    }))).toMap ++ baseModels.toMap ++ dependentModels.toMap
  }
}